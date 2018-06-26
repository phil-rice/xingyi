package core;

import com.sun.net.httpserver.HttpHandler;

public interface PathAndHandler extends HttpHandler {
    String path();



}
