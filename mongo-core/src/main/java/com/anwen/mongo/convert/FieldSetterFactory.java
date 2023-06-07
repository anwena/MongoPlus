package com.anwen.mongo.convert;

import java.lang.reflect.Field;
import java.util.HashMap;
import java.util.Map;

/**
 * @Description: 根据属性类型创建对应的属性设置器
 * @BelongsProject: mongo
 * @BelongsPackage: com.anwen.mongo.convert
 * @Author: JiaChaoYang
 * @CreateTime: 2023-06-07 01:06
 * @Version: 1.0
 */
public class FieldSetterFactory {
    private static final Map<String, FieldSetter> FIELD_SETTERS = new HashMap<>();

    static {
        FIELD_SETTERS.put("String", new StringFieldSetter());
        FIELD_SETTERS.put("int", new IntFieldSetter());
        FIELD_SETTERS.put("Integer", new IntFieldSetter());
        FIELD_SETTERS.put("long", new LongFieldSetter());
        FIELD_SETTERS.put("Long", new LongFieldSetter());
        FIELD_SETTERS.put("float", new FloatFieldSetter());
        FIELD_SETTERS.put("Float", new FloatFieldSetter());
        FIELD_SETTERS.put("double", new DoubleFieldSetter());
        FIELD_SETTERS.put("Double", new DoubleFieldSetter());
        FIELD_SETTERS.put("boolean", new BooleanFieldSetter());
        FIELD_SETTERS.put("Boolean", new BooleanFieldSetter());
    }

    public static FieldSetter createSetter(String fieldType) {
        FieldSetter setter = FIELD_SETTERS.get(fieldType);
        if (setter == null) {
            throw new IllegalArgumentException("Unsupported field type: " + fieldType);
        }
        return setter;
    }
}

/**
 * 字符串类型属性设置器。
 */
class StringFieldSetter implements FieldSetter {
    @Override
    public void setFieldValue(Object target, Field field, Object value) throws IllegalAccessException {
        field.set(target, value.toString());
    }
}

/**
 * 整数类型属性设置器。
 */
class IntFieldSetter implements FieldSetter {
    @Override
    public void setFieldValue(Object target, Field field, Object value) throws IllegalAccessException {
        field.setInt(target, Integer.parseInt(value.toString()));
    }
}

/**
 * 长整型属性设置器。
 */
class LongFieldSetter implements FieldSetter {
    @Override
    public void setFieldValue(Object target, Field field, Object value) throws IllegalAccessException {
        field.setLong(target, Long.parseLong(value.toString()));
    }
}

/**
 * 浮点数类型属性设置器。
 */
class FloatFieldSetter implements FieldSetter {
    @Override
    public void setFieldValue(Object target, Field field, Object value) throws IllegalAccessException {
        field.setFloat(target, Float.parseFloat(value.toString()));
    }
}

/**
 * 双精度浮点数类型属性设置器。
 */
class DoubleFieldSetter implements FieldSetter {
    @Override
    public void setFieldValue(Object target, Field field, Object value) throws IllegalAccessException {
        field.setDouble(target, Double.parseDouble(value.toString()));
    }
}

/**
 * 布尔类型属性设置器。
 */
class BooleanFieldSetter implements FieldSetter {
    @Override
    public void setFieldValue(Object target, Field field, Object value) throws IllegalAccessException {
        field.setBoolean(target, Boolean.parseBoolean(value.toString()));
    }
}
