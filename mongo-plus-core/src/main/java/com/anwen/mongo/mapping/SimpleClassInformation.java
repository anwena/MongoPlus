package com.anwen.mongo.mapping;

import com.anwen.mongo.cache.global.ClassInformationCache;
import com.anwen.mongo.domain.MongoPlusFieldException;
import com.anwen.mongo.toolkit.CollUtil;

import java.lang.annotation.Annotation;
import java.lang.reflect.InvocationTargetException;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Class的一些信息和操作
 * @author JiaChaoYang
 * @date 2024/4/16 下午9:04
*/
public class SimpleClassInformation<T> implements ClassInformation {

    /**
     * 实例
     * @author JiaChaoYang
     * @date 2024/4/16 下午9:58
    */
    private final T instance;

    /**
     * 实例的Class
     * @author JiaChaoYang
     * @date 2024/4/16 下午9:58
    */
    private final Class<?> clazz;

    /**
     * 实例的所有Field
     * @author JiaChaoYang
     * @date 2024/4/16 下午9:58
    */
    private final List<FieldInformation> fieldList = new ArrayList<>();

    /**
     * 实例的某个注解的Field
     * @author JiaChaoYang
     * @date 2024/4/16 下午9:58
    */
    private final Map<Class<? extends Annotation>, List<FieldInformation>> annotationFieldMap = new HashMap<>();

    private SimpleClassInformation(T instance) {
        this.instance = instance;
        this.clazz = getInstanceClass();
    }

    private Class<?> getInstanceClass(){
        Class<?> instanceClass = instance.getClass();
        if (instanceClass.isAnonymousClass()){
            instanceClass = instanceClass.getSuperclass();
        }
        return instanceClass;
    }

    @Override
    public Class<?> getClazz() {
        return clazz;
    }

    @Override
    public Boolean isMap() {
        return Map.class.isAssignableFrom(clazz);
    }

    @Override
    public Boolean isCollection() {
        return Collection.class.isAssignableFrom(clazz);
    }

    public T getInstance() {
        return instance;
    }

    public static <T> ClassInformation of(T instance){
        Class<?> clazz = instance.getClass();
        return Optional.ofNullable(ClassInformationCache.classInformationMap.get(clazz)).orElseGet(() -> {
            SimpleClassInformation<T> simpleClassInformation = new SimpleClassInformation<>(instance);
            ClassInformationCache.classInformationMap.put(clazz, simpleClassInformation);
            return simpleClassInformation;
        });
    }

    public static <T> ClassInformation of(Class<T> clazz){
        try {
            return of(clazz.getDeclaredConstructor().newInstance());
        } catch (InstantiationException | IllegalAccessException | InvocationTargetException | NoSuchMethodException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public List<FieldInformation> getFields(){
        if (CollUtil.isEmpty(fieldList)){
            Arrays.stream(clazz.getDeclaredFields()).forEach(field -> {
                field.setAccessible(true);
                fieldList.add(new SimpleFieldInformation<>(instance,field));
            });
            getSupperFields(clazz.getSuperclass());
        }
        return this.fieldList;
    }

    @Override
    public List<FieldInformation> getAnnotationFields(Class<? extends Annotation> annotationClass){
        if (!annotationFieldMap.containsKey(annotationClass)){
            annotationFieldMap.put(annotationClass,getFields().stream().filter(field -> field.getField().getAnnotation(annotationClass) != null).collect(Collectors.toList()));
        }
        return annotationFieldMap.get(annotationClass);
    }

    @Override
    public FieldInformation getAnnotationField(Class<? extends Annotation> annotationClass,String nullMessage){
        List<FieldInformation> fieldList = getAnnotationFields(annotationClass);
        if (CollUtil.isEmpty(fieldList)){
            throw new MongoPlusFieldException(nullMessage);
        }
        return fieldList.get(0);
    }

    @Override
    public Object getAnnotationFieldValue(Class<? extends Annotation> annotationClass,String nullMessage){
        return getAnnotationField(annotationClass,nullMessage).getValue();
    }

    private void getSupperFields(Class<?> clazz){
        if (clazz != null && !clazz.equals(Object.class)){
            Arrays.asList(clazz.getDeclaredFields()).forEach(field -> {
                fieldList.add(new SimpleFieldInformation<>(instance,field));
            });
            getSupperFields(clazz.getSuperclass());
        }
    }

}
