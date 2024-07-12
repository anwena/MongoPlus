package com.anwen.mongo.handlers.collection;

import com.anwen.mongo.annotation.collection.CollectionName;
import com.anwen.mongo.enums.CollectionNameConvertEnum;
import com.anwen.mongo.toolkit.StringUtils;

import java.util.function.Function;

import static com.anwen.mongo.enums.CollectionNameConvertEnum.ALL_CHAR_LOWERCASE;

/**
 * 便捷的注解操作
 *
 * @author anwen
 * @date 2024/7/10 下午3:52
 */
public class AnnotationOperate implements AnnotationHandler {

    private static AnnotationHandler ANNOTATION_HANDLER_INSTANCE = new AnnotationOperate();

    private static CollectionNameConvertEnum collectionNameConvertEnum = ALL_CHAR_LOWERCASE;

    public static AnnotationHandler getAnnotationHandler() {
        return ANNOTATION_HANDLER_INSTANCE;
    }

    public static void setAnnotationHandler(AnnotationHandler annotationHandler) {
        ANNOTATION_HANDLER_INSTANCE = annotationHandler;
    }

    public static void setCollectionNameConvertEnum(CollectionNameConvertEnum collectionNameConvertEnum) {
        AnnotationOperate.collectionNameConvertEnum = collectionNameConvertEnum;
    }

    public static String getCollectionName(Class<?> clazz){
        String collectionName = getCollectionInfo(clazz.getAnnotation(CollectionName.class),CollectionName::value);
        if (StringUtils.isBlank(collectionName)){
            collectionName = convert(clazz);
        }
        return collectionName;
    }

    public static <R> R getCollectionInfo(CollectionName collectionName, Function<? super CollectionName,? extends R> func){
        if (collectionName == null){
            return null;
        }
        return ANNOTATION_HANDLER_INSTANCE.getProperty(collectionName,func);
    }

    public static String getDatabase(Class<?> clazz){
        return getCollectionInfo(clazz.getAnnotation(CollectionName.class),CollectionName::database);
    }

    private static String convert(Class<?> clazz){
        String collectionName = null;
        switch (collectionNameConvertEnum) {
            case ALL_CHAR_LOWERCASE: collectionName = clazz.getSimpleName().toLowerCase(); break;
            case FIRST_CHAR_LOWERCASE: collectionName = StringUtils.firstCharToLowerCase(clazz.getSimpleName()); break;
            case CLASS_NAME: collectionName = clazz.getSimpleName(); break;
            case CAMEL_TO_UNDERLINE: collectionName = StringUtils.camelToUnderline(clazz.getSimpleName()); break;
        }
        return collectionName;
    }

}
