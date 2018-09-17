package one.xingyi.javaserver;

import javax.net.ssl.KeyManagerFactory;
import javax.net.ssl.SSLContext;
import javax.net.ssl.TrustManagerFactory;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.UnrecoverableKeyException;
import java.security.cert.CertificateException;

public class SSLContextFactory {

    public static String keyStore = "javax.net.ssl.keyStore";
    public static String keyStorePass = "javax.net.ssl.keyStorePassword";
    public static String truststore = "javax.net.ssl.trustStore";
    public static String trustStorePassword = "javax.net.ssl.trustStorePassword";

    private static String get(String key) {
        return System.getProperty(key);
    }

    private static void fail(String reason, Throwable cause) {
        throw new IllegalStateException("Failed to initialise SSL because" + reason +
                "\nKeyStore " + get(keyStore) +
                "\nkeyStorePass " + (get(keyStorePass) != null) +
                "\nTruststore " + get(truststore) +
                "\nTrustStorePassword " + get(trustStorePassword), cause);
    }

    public static KeyStore getKeyStore(String keyStoreProperty, String keyStorePassProperty) throws Exception {
        KeyStore ks = KeyStore.getInstance("JKS");
        InputStream fis = new FileInputStream(get(keyStoreProperty));
        if (fis == null) fail("Cannot load keystore", null);
        ks.load(fis, get(keyStorePassProperty).toCharArray());
        return ks;
    }

    private static SSLContext actualDefault;

    public static SSLContext defaultSslContext() throws Exception {
        if (actualDefault == null)
            try {
                actualDefault = SSLContext.getInstance("TLS");

                // setup the key manager factory
                KeyManagerFactory kmf = KeyManagerFactory.getInstance("SunX509");
                kmf.init(getKeyStore(keyStore, keyStorePass), "mypass".toCharArray());

                // setup the trust manager factory
                TrustManagerFactory tmf = TrustManagerFactory.getInstance("SunX509");
                tmf.init(getKeyStore(truststore, trustStorePassword));

                // setup the HTTPS context and parameters
                actualDefault.init(kmf.getKeyManagers(), tmf.getTrustManagers(), null);
            } catch (Exception e) {
                fail("Cannot make defaultSslContext", e);
                throw e;//will not be executed but needed by compiler
            }
        return actualDefault;
    }
}
