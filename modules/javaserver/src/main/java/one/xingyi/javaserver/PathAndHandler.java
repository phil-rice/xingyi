package one.xingyi.javaserver;

import com.sun.net.httpserver.HttpHandler;

public interface PathAndHandler extends HttpHandler {
    String path();



}
