module redis-tcp-client

go 1.14

require (
	github.com/go-redis/redis/v8 v8.5.0
	github.com/mashingan/localredis v0.0.0
)

replace github.com/mashingan/localredis v0.0.0 => ../../localredis
