package com.anwen.mongo.mapping;

import com.anwen.mongo.cache.global.TypeInformationCache;
import com.anwen.mongo.domain.MongoPlusFieldException;
import com.anwen.mongo.toolkit.ArrayUtils;
import com.anwen.mongo.toolkit.CollUtil;

import java.lang.annotation.Annotation;
import java.lang.reflect.Modifier;
import java.lang.reflect.Type;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Class的一些信息和操作
 * @author JiaChaoYang
 * @date 2024/4/16 下午9:04
*/
public class SimpleTypeInformation<T> implements TypeInformation {

    /**
     * 实例
     * @author JiaChaoYang
     * @date 2024/4/16 下午9:58
    */
    private T instance;

    /**
     * 实例的Class
     * @author JiaChaoYang
     * @date 2024/4/16 下午9:58
    */
    private final Class<?> clazz;

    private Type[] types;

    private final Map<String,FieldInformation> fieldMap = new HashMap<>();

    /**
     * 实例的所有Field
     * @author JiaChaoYang
     * @date 2024/4/16 下午9:58
    */
    private final List<FieldInformation> fieldList = new ArrayList<>();

    private final SimpleTypeHolder simpleTypeHolder = new SimpleTypeHolder();

    /**
     * 实例的某个注解的Field
     * @author JiaChaoYang
     * @date 2024/4/16 下午9:58
    */
    private final Map<Class<? extends Annotation>, List<FieldInformation>> annotationFieldMap = new HashMap<>();

    private SimpleTypeInformation(T instance,Class<?> clazz) {
        this.instance = instance;
        this.clazz = clazz;
    }

    protected SimpleTypeInformation(T instance,Type[] types) {
        this.instance = instance;
        this.clazz = getInstanceClass(instance);
        this.types = types;
    }

    private static Class<?> getInstanceClass(Object instance){
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

    @Override
    public Boolean isSimpleType() {
        return simpleTypeHolder.isSimpleType(clazz);
    }

    @Override
    public Type[] getType() {
        if (ArrayUtils.isEmpty(types)){
            types = clazz.getTypeParameters();
        }
        return types;
    }

    @Override
    public T getInstance() {
        return instance;
    }

    public static <T> TypeInformation of(T instance){
        return new SimpleTypeInformation<>(instance,getInstanceClass(instance));
    }

    @Override
    @SuppressWarnings("unchecked")
    public void setInstance(Object instance) {
        this.instance = (T) instance;
    }

    @Override
    public List<FieldInformation> getFields(){
        if (CollUtil.isEmpty(fieldList)){
            Arrays.stream(clazz.getDeclaredFields()).forEach(field -> {
                field.setAccessible(true);
                if (Modifier.isStatic(field.getModifiers())){
                    return;
                }
                fieldList.add(new SimpleFieldInformation<>(instance,field));
            });
            getSupperFields(clazz.getSuperclass());
        }
        return this.fieldList;
    }

    @Override
    public FieldInformation getField(String fieldName) {
        if (!fieldMap.containsKey(fieldName)) {
            try {
                fieldMap.put(fieldName, new SimpleFieldInformation<>(instance, clazz.getField(fieldName)));
            } catch (NoSuchFieldException e) {
                throw new RuntimeException(e);
            }
        }
        return fieldMap.get(fieldName);
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
    public FieldInformation getAnnotationField(Class<? extends Annotation> annotationClass) {
        List<FieldInformation> fieldList = getAnnotationFields(annotationClass);
        if (CollUtil.isEmpty(fieldList)){
            return null;
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
                field.setAccessible(true);
                if (Modifier.isStatic(field.getModifiers())){
                    return;
                }
                fieldList.add(new SimpleFieldInformation<>(instance,field));
            });
            getSupperFields(clazz.getSuperclass());
        }
    }

}
