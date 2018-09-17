package one.xingyi.javaserver;

import java.io.*;

public class Streams {

    public static String readAll(InputStream stream) throws IOException {
        BufferedReader in = new BufferedReader(new InputStreamReader(stream));
        try {
            StringBuilder result = new StringBuilder();
            String inputLine;
            while ((inputLine = in.readLine()) != null)
                result.append(inputLine);
            return result.toString();
        } finally {
            in.close();
        }
    }



    public static void sendAll(OutputStream stream, byte[] bytes) throws IOException {
        try {
            stream.write(bytes);
        } finally {
            stream.close();
        }
    }
}
