FROM python:3.11.4-bullseye

WORKDIR /app

RUN pip install black

RUN apt update && \
    apt install -y bsdmainutils