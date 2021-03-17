FROM nginx:1.19-alpine
LABEL maintainer="Nyk Ma <i@nyk.ma>"

COPY ./public /usr/share/nginx/html

ENV NGINX_HOST=nyk.ma
