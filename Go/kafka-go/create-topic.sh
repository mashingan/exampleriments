# podman-compose exec broker \
#   kafka-topics --create \
#     --topic purchases \
#     --bootstrap-server localhost:9092 \
#     --replication-factor 1 \
#     --partitions 1
docker exec broker \
  kafka-topics --create \
    --topic purchases \
    --bootstrap-server localhost:9092 \
    --replication-factor 1 \
    --partitions 1