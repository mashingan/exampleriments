# Tutorial Kafka with Go

Ref: https://developer.confluent.io/get-started/go/

# Podman
First of all, when we use `podman` instead of `docker`.  
We need to make sure `podman-compose` is installed.  
On Ubuntu the command will be

```bash
sudo apt install podman-compose
```

Change any instance of `docker compose` with `podman-compose`.

# Running

The summary of how running it can be listed in below commands:

```bash
docker-compose build
docker-compose up -d
./build.sh
./create-topic.sh
./run
```

~In some other mode, let's say we're not in Windows (which unfortunate) 
so we don't have Bash or shell command available, we can try leverage
Go stdlib exec to run it so above can be done like~  
Nope, apparently not working.

```bash
docker-compose build
docker-compose up -d
go run cmd/build/main.go
go run cmd/create-topic/main.go
go run cmd/run/main.go
```

To stop, simply `docker-compose down` will do the job.