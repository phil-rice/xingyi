package core.wrapped;

public class WrappedException extends RuntimeException {
    public static <T> T wrap(WrappedBlock<T> block) {
        try {
            return block.execute();
        } catch (RuntimeException e) {
            throw e;
        } catch (Exception e) {
            throw new WrappedException(e);
        }
    }

    public static void wrapCode(WrappedCode code) {
        try {
            code.execute();
        } catch (RuntimeException e) {
            throw e;
        } catch (Exception e) {
            throw new WrappedException(e);
        }
    }

    public WrappedException(Exception e) {
        super(e);
    }
}
