package core;

import java.util.Objects;

public class SimpleHttpResponse {

    public static SimpleHttpResponse json(String... idAndvalues) {
        return new SimpleHttpResponse(200, "application/json", Json.asJson((Object[]) idAndvalues));
    }

    public static SimpleHttpResponse notFound(String message) {
        return new SimpleHttpResponse(404, "text/plain", message);
    }

    private int statusCode;
    private String contentType;
    private String body;

    public SimpleHttpResponse(int statusCode, String contentType, String body) {
        this.statusCode = statusCode;
        this.contentType = contentType;
        this.body = body;
    }

    public int getStatusCode() {
        return statusCode;
    }

    public String getContentType() {
        return contentType;
    }

    public String getBody() {
        return body;
    }


    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        SimpleHttpResponse that = (SimpleHttpResponse) o;
        return statusCode == that.statusCode &&
                Objects.equals(contentType, that.contentType) &&
                Objects.equals(body, that.body);
    }

    @Override
    public int hashCode() {

        return Objects.hash(statusCode, contentType, body);
    }

    @Override
    public String toString() {
        return "SimpleHttpResponse{" +
                "statusCode=" + statusCode +
                ", contentType='" + contentType + '\'' +
                ", body='" + body + '\'' +
                '}';
    }
}
