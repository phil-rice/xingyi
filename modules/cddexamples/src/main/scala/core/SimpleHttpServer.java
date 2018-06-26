package core;


import com.sun.net.httpserver.HttpServer;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.util.concurrent.Executor;

public class SimpleHttpServer {

    private final HttpServer server;

    public SimpleHttpServer(int port, Executor executor, PathAndHandler... handlers) throws IOException {
        this.server = HttpServer.create(new InetSocketAddress(port), 0);
        for (PathAndHandler pathAndHandler : handlers) {
            server.createContext(pathAndHandler.path(), pathAndHandler);
        }
        server.setExecutor(executor);
    }

    public void start() {
        server.start();
    }

    public void stop() {
        server.stop(0);
    }


}
