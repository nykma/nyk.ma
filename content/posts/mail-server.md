---
title: 快速搭建全功能 Mail server
categories: ["software"]
draft: false
author: ["Nyk Ma"]
date: 2017-05-04
---

## 目标

我要一个拥有无穷多个alias的邮箱 `nykma@example.com`，注册每个网站都来一个不同的Email地址，从而知道哪个服务把我邮箱卖了。

## 开工

准备好DNS：

```bash
;; 我记得这个字段尽量不要CNAME，好像会出问题
mail-server.example.com.  IN A     233.233.233.233
example.com.               IN MX 10 mail-server.example.com.
```

用 LetsEncrypt 给 `mail-server.example.com` 申请一个证书。

然后 `git clone https://github.com/tomav/docker-mailserver.git` 。

然后参考里面的 `docker-compose.yml.dist`，写一个 `docker-compose.yml`，需要改动：

- `domainname`，本例中 `example.com`
- `hostname`，本例中 `mail-server`
- ENV部分增加一条 `SSL_TYPE=letsencrypt`
- 将宿主机的 `/etc/letsencrypt` volumns 进 container 里的相同位置

    > 该文件夹里有 `certs`、`csr`、`keys`、`live`、`renewal` 之类的东西

    > 如果你不是用的 `certbot-auto`，路径可能不同。

然后

```bash
./setup.sh email add nykma@example.com PassW0rd!
./setup.sh email list # 应该能看到 nykma@example.com
./setup.sh alias add @example.com nykma@example.com
./setup.sh alias list # 应该能看到 @example.com nykma@example.com,
docker run --rm -v "$(pwd)/config":/tmp/docker-mailserver -it tvial/docker-mailserver:latest generate-dkim-config
```

然后去 `./config/opendkim/keys/example.com` 找个txt，照着内容添加对应的DNS的TXT记录。注意你最终填进DNS提供商里的TXT应该是这文件里两个字符串拼起来的结果，结果中是没有引号的。类似 `v=DKIM1; k=rsa; p=fleiwflki....` 这样。

然后就可以 `docker-compose up` 了。

接下来随便用个什么邮件客户端，添加帐号：

- 用户名是完整Email地址
- IMAP和SMTP服务器都填 `mail-server.example.com`
- IMAP 端口 143，STARTTLS，普通密码验证
- SMTP 端口 587，STARTTLS，普通密码验证

应该可以收发件了。

## Troubleshooting

### 初次连接服务器时提示证书错误

查看一下服务器给的证书，如果是Dovecot自签证书的话说明LetsEncrypt没配置对，如果是提示证书域名不正确的话说明DNS没配置对。

### alias没生效，邮件服务器提示查无此帐号

对 alias 的改动是在重启 container 之后才会生效的。

检查 `./setup.sh alias list` 的结果，应该如我上面例子的预期结果那样。如果不对，手动在 `./config/postfix-virtual.cf` 里加上这行结果，并重启container。


## What's next

这个镜像的功能很简单，但完成得不错。如果你需要更强大的邮件账户管理，甚至Web前端的话，可以看看 [Mailu 项目](https://github.com/Mailu/Mailu) 。

## References

- [Configure SSL · tomav/docker-mailserver Wiki](https://github.com/tomav/docker-mailserver/wiki/Configure-SSL)
