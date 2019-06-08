package core.wrapped;

public interface WrappedBlock<T> {
    T execute() throws Exception;
}
