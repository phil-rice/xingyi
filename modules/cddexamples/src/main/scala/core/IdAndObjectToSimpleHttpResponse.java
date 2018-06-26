package core;

public interface IdAndObjectToSimpleHttpResponse<Request, Result> {
    SimpleHttpResponse makeHttpResponse(Request request, Result result);
}
