package one.xingyi.javaserver;

import com.sun.net.httpserver.HttpExchange;

import java.io.IOException;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class    HttpUtils {

    public static ExecutorService makeDefaultExecutor() {
        return Executors.newFixedThreadPool(100);
    }

    public static void write(HttpExchange exchange, SimpleHttpResponse response) throws IOException {
        exchange.getResponseHeaders().set("content-type", response.getContentType());
        byte[] bytes = response.getBody().getBytes("UTF-8");
        exchange.sendResponseHeaders(response.getStatusCode(), bytes.length);
        Streams.sendAll(exchange.getResponseBody(), bytes);
    }

    public static void process(HttpExchange exchange, Callable<SimpleHttpResponse> callable) throws IOException {
        try {
            HttpUtils.write(exchange, callable.call());
        } catch (Exception e) {
            HttpUtils.write(exchange, new SimpleHttpResponse(500, "text/plain", e.getClass().getName() + "\n" + e.getMessage()));
        }
    }


    public static String id(HttpExchange httpExchange, String prefix) {
        return httpExchange.getRequestURI().getPath().substring(prefix.length() + 1);
    }

}
