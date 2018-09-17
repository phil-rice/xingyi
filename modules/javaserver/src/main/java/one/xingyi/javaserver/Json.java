package one.xingyi.javaserver;

public class Json {

    public static String asJson(Object... idAndValues) {
        if (idAndValues.length % 2 != 0)
            throw new IllegalArgumentException("Needed an even number of parameters, had " + idAndValues.length);
        if (idAndValues.length == 0) throw new IllegalArgumentException("Must have some arguements");
        StringBuilder builder = new StringBuilder("{");
        for (int i = 0; i < idAndValues.length; i += 2) {
            if (i != 0) builder.append(",");
            String key = idAndValues[i + 0].toString();
            String value = idAndValues[i + 1].toString();
            builder.append("\"" + key + "\"");
            builder.append(":");
            builder.append("\"" + value + "\"");
        }
        builder.append("}");
        return builder.toString();
    }

    public static String valueFor(String json, String id) {
        int idIndex = json.indexOf(id);
        if (idIndex == -1) throw new IllegalArgumentException("id " + id + " not found in " + json);
        int colon = json.indexOf(":", idIndex);
        int firstQuotes = json.indexOf("\"", colon);
        int secondQuotes = json.indexOf("\"", firstQuotes + 1);

        return json.substring(firstQuotes + 1, secondQuotes);
    }

    public static String valueForRemovingJunk(String json, String id){
        return valueFor(removeJunk(json), id);
    }

    public static String junk = "}}]!}";

    public static String addJunk(String json) {
        return junk + json;
    }

    public static String removeJunk(String json) {
        if (json.startsWith(junk)) return json.substring(junk.length());
        throw new IllegalArgumentException("Json should start with '" + junk);
    }
}
