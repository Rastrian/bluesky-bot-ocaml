# Bluesky Bot

This is an automated bot for Bluesky, built with OCaml. It automatically follows/unfollows users and reposts mentions based on specific criteria.

## Features

- Automatically follows new followers and unfollows users who have unfollowed ([unfollow functionatily not working yet](https://bsky.app/profile/rastrian.dev/post/3l2zxgq7mkn23), by that handled via discord webhook just for history)
- Reposts mentions that include "cc @userhandle" in the text
- Uses Redis for state management
- Configurable through environment variables
- Dockerized for easy deployment

## Setup

1. Copy the `.env-example` file to `.env` and update it with your Bluesky credentials and preferences:
   ```
   cp .env-example .env
   ```

2. Open the `.env` file and fill in your details.

## Usage

To start the bot, run:

```
docker-compose up --build
```

This command will build the Docker image and start the bot along with a Redis instance.

To run the bot in the background, use:

```
docker-compose up -d --build
```

To stop the bot:

```
docker-compose down
```

## Logs

To view the logs of the running bot:

```
docker-compose logs -f app
```