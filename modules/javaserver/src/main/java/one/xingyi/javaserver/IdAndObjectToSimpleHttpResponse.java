package one.xingyi.javaserver;

public interface IdAndObjectToSimpleHttpResponse<Request, Result> {
    SimpleHttpResponse makeHttpResponse(Request request, Result result);
}
