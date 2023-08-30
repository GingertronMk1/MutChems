FROM python:3.11.4-bullseye

WORKDIR /app

RUN pip install \
    black~=23.7 \
    mypy~=1.5 \
    pylint~=2.17

RUN apt update && \
    apt install -y \
        bsdmainutils \
        jq
