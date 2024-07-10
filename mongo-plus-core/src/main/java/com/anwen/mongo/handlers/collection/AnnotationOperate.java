package com.anwen.mongo.handlers.collection;

import com.anwen.mongo.annotation.collection.CollectionName;

/**
 * 便捷的注解操作
 *
 * @author anwen
 * @date 2024/7/10 下午3:52
 */
public class AnnotationOperate implements AnnotationHandler {

    private static AnnotationHandler ANNOTATION_HANDLER_INSTANCE = new AnnotationOperate();

    public static AnnotationHandler getAnnotationHandler() {
        return ANNOTATION_HANDLER_INSTANCE;
    }

    public static void setAnnotationHandler(AnnotationHandler annotationHandler) {
        ANNOTATION_HANDLER_INSTANCE = annotationHandler;
    }

    public String getCollectionName(CollectionName collectionName){
        return ANNOTATION_HANDLER_INSTANCE.getProperty(collectionName,CollectionName::value);
    }
}
