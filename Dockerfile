FROM python:3.11.4-bullseye

WORKDIR /app

RUN pip install black
RUN pip install pylint

RUN apt update && \
    apt install -y bsdmainutils