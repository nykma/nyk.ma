---
title: 本站技术栈（V3）
subtitle: Rails / PostgreSQL / Sidekiq
categories: ["network"]
draft: false
author: ["Nyk Ma"]
date: 2016-03-14
---

最后还是用回了我最熟悉的Rails框架，能从容应对我对自己网站日益增长的各种前后端需求。

代码就不开源了，没啥意思。你写得比我好。

## 器件选型

这套工具链是我用得最顺手的：

- [Ruby on Rails][ror]
    - [pg][pg] (`PostgreSQL`)
    - paperclip
    - grape
- [dalli][dalli] (`memcached`)
- [Sidekiq][sidekiq] (`redis`)
- [searchkick][searchkick] (`elasticsearch`)
- [capistrano][capistrano]
    - [whenever][whenever]

## 实现功能

既然上了RoR，咱就豪放点，不用像[Sinatra实现](https://nyk.moe/archives/network/tech-stack)那样缩手缩脚的了，直接上Model。

### /archives

#### Model

- migration

```ruby
# db/schema.rb
create_table "articles", force: :cascade do |t|
  t.string   "title"
  t.string   "subtitle"
  t.string   "type"
  t.text     "markdown_body"
  t.text     "body"
  t.boolean  "hidden",        default: false
  t.datetime "created_at",                    null: false
  t.datetime "updated_at",                    null: false
  t.text     "brief"
  t.boolean  "index_page",    default: false
  t.string   "file_path"
  t.string   "link"
  t.string   "category"
end
```

把`kramdown`编译好了的markdown body直接扔进数据库（`body`字段）。其它能初始化好的字段全部存起来，供以后用。

> 建立搜索索引时现场建立一个`plain_text`，不另设字段。

- Article

先来点验证：

```ruby
class Article < ActiveRecord::Base
  validates :title, :markdown_body, :category, :file_path, presence: true
  validates :link, uniqueness: true
  validates :brief, length: { maximum: 60 }
end
```

想到 `category` 应当是个枚举型：

```ruby
class Article < ActiveRecord::Base
  extend Enumerize

  enumerize :category, in: {
    Default: "default",
    ACG: "acg",
    Software: "software",
    Hardware: "hardware",
    Game: "game",
    Network: "network",
    SoC: "soc",
  }, default: :Default
end
```

从源文件获取meta、编译到HTML均应当自动触发：

```ruby
class Article < ActiveRecord::Base
  before_update :init_article
  before_create :init_article

  private
  def init_article
    return if markdown_body.blank?
    self.body = Kramdown::Document.new(
      markdown_body,
      input: 'GFM',
      syntax_highlighter: :coderay,
      syntax_highlighter_opts: {
        span: { disable: true },
        block: { line_numbers: false }
      }
    ).to_html
    self.brief = self.body.gsub(/<.*?>/, "")[0,50] + "..." if self.brief.blank?
    self.link = "/#{self.category.value}/#{File.basename(self.file_path)[0..-4]}"
  end
end
```

- Article#list

生成目录供右上角的 ToC 使用。

```ruby
class Article < ActiveRecord::Base
  def self.list
    # 这玩意儿需要遍历数据库且不经常变动，做个缓存
    Rails.cache.fetch(self.cache_key, expires_in: 7.days) do
      result = {  }
      Article.find_each do |article|
        result[article.category.to_sym] = [] if result[article.category.to_sym].blank?
        result[article.category.to_sym] << {
          id: article.id,
          title: article.title,
          link: article.link,
        }
      end
      result
      # { :network => [{ id: 39232, title: "本站技术栈（V3）", link: '/network/tech-stack-ror' }] }
    end
  end

  # 其中，
  def self.cache_key
    "{article.list}" + Article.order("updated_at DESC").first.updated_at.to_i.to_s
  end
end
```

#### Controller、View

erb 自带了 `content_for` 这个注入信息的功能，不用像上次那样纠结了。

```ruby
Rails.application.routes.draw do
  get 'archives/:category/:title' => "archives#article"
end
```

```ruby
class ArchivesController < ApplicationController
  def article
    @page = "archives"
    @article = Article.find_by!(link: "/#{params[:category]}/#{params[:title]}")
    @list = Article.list
    @footer = {
      created_at: @article.created_at.to_s,
      updated_at: @article.updated_at.to_s,
    }.to_s
  end
end
```

```erb
<!-- app/views/archives/article.erb -->

<div class="row">
  <div class="columns large-12 small-12">
    <article>
      <h1><%= @article.title %>
      <% if @article.subtitle %>
        <small><%= @article.subtitle %></small>
      <% end %>
      <hr>
      </h1>

      <content>
        <%=raw @article.body %>
      </content>
    </article>
  </div>
</div>
```

#### 自动更新

`whenever` 把 `cron` 反人类的语法封装得非常好：

```ruby
# config/schedule.rb

every 1.day, :at => '3:00 am' do
  runner "Article.find_each(&:update_from_file)"
  # Article.find_each { |article| article.update_from_file }
end
```

没做**监视文件改动**的 hot-reload，没想到什么好办法。找了一圈 `Guard` 的资料感觉它太重……


## Twitter archive

我写一个使用API自动清理我的老推的机器人。流程是先更新新推到数据库，再删除早于一个时间窗口的推。

用到了一个强大的gem：[`twitter`](https://rubygems.org/gems/twitter)。

> 它还有一个CLI工具，就叫 [`t`](https://rubygems.org/gems/t)。

### Migration

主要是参考Twitter官方的archive导出格式来的，增加了fav和RT的count：

```ruby
create_table "tweets", force: :cascade do |t|
  t.string   "tweet_id"
  t.datetime "timestamp"
  t.string   "source"
  t.string   "text"
  t.string   "in_reply_to_status_id"
  t.string   "in_reply_to_user_id"
  t.string   "retweeted_status_id"
  t.string   "retweeted_status_user_id"
  t.datetime "retweeted_status_timestamp"
  t.string   "expanded_urls"
  t.boolean  "deleted",                    default: false
  t.datetime "created_at",                                 null: false
  t.datetime "updated_at",                                 null: false
  t.integer  "favorite_count",             default: 0
  t.integer  "retweet_count",              default: 0
end
```

[ror]: https://rubyonrails.org
[pg]: https://bitbucket.org/ged/ruby-pg/wiki/Home
[dalli]: https://github.com/petergoldstein/dalli
[sidekiq]: http://sidekiq.org
[searchkick]: https://github.com/ankane/searchkick
[capistrano]: http://capistranorb.com/
[whenever]: https://github.com/javan/whenever
