package one.xingyi.core.script;

import java.lang.annotation.*;

@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Inherited
public @interface XingYiInterface {
    public  Class<?>[] clazzes();
}
