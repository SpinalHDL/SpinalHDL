package spinal.core;

import java.lang.reflect.Field;

/**
 * Created by PIC32F_USER on 14/06/2016.
 */
public class AnnotationUtils {
    public static boolean isDontName(Field f){
        return f.isAnnotationPresent(dontName.class);
    }
}
