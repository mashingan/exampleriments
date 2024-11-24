FROM golang:1.14.0-alpine3.11 AS builder

RUN apk update && apk add --no-cache git sqlite-libs sqlite-dev
RUN apk add --no-cache build-base
WORKDIR /app

COPY . .

RUN  go build -o main -ldflags "-linkmode external -extldflags -static"


FROM scratch

#RUN apk add --no-cache tzdata

#ENV URL_SELFREGISTRATION_GRPC=lkp-self-registration-grpc.lkp:8181
#COPY --from=builder /app/.env /app/
COPY --from=builder /app/main /app/

EXPOSE 9191

ENTRYPOINT [ "/app/main" ]
