package vertx.rdr.com.starter;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.Promise;
import io.vertx.core.MultiMap;
import io.vertx.ext.web.Router;
import io.vertx.core.json.JsonObject;
import java.lang.String;

public class MainVerticle extends AbstractVerticle {

  @Override
  public void start(Promise<Void> startPromise) throws Exception {
    /*
    vertx.createHttpServer().requestHandler(req -> {
      req.response()
        .putHeader("content-type", "text/plain")
        .end("Hello from Vert.x!");
    }).listen(8888, http -> {
      if (http.succeeded()) {
        startPromise.complete();
        System.out.println("HTTP server started on port 8888");
      } else {
        startPromise.fail(http.cause());
      }
    });
    */
    Router router = Router.router(vertx);
    router.route().handler(context -> {
      String address = context.request().connection().remoteAddress().toString();
      MultiMap queryParams = context.queryParams();
      String name = queryParams.contains("name") ? queryParams.get("name") : "unknown";
      context.json(
        new JsonObject()
        .put("name", name)
        .put("address", address)
        .put("message", "hello " + name + " connected from " +  address)
      );
    });

    vertx.createHttpServer()
    .requestHandler(router)
    .listen(8888)
    .onSuccess(server ->
    System.out.println(
      "HTTP server started on port " + server.actualPort()
    ));
  }
}
