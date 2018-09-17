package one.xingyi.javaserver;


import com.sun.net.httpserver.HttpsConfigurator;
import com.sun.net.httpserver.HttpsParameters;
import com.sun.net.httpserver.HttpsServer;

import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLParameters;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.util.concurrent.Executor;

public class SimpleHttpsServer {

    private boolean useClientAuth = true;
    private final HttpsServer server;

    public SimpleHttpsServer(int port, Executor executor, SSLContext sslContext, PathAndHandler... handlers) throws IOException {
        this.server = HttpsServer.create(new InetSocketAddress(port), 0);
              server.setHttpsConfigurator(new HttpsConfigurator(sslContext) {
            public void configure(HttpsParameters params) {
                try {
                    SSLParameters sslParams = sslContext.getDefaultSSLParameters();
                    sslParams.setNeedClientAuth(useClientAuth);
                    params.setNeedClientAuth(useClientAuth);
                    params.setSSLParameters(sslParams);
                } catch (Exception ex) {
                    ex.printStackTrace();
                }
            }
        });
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
