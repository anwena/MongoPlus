package com.anwen.mongo.codec;

import org.bson.BsonReader;
import org.bson.BsonType;
import org.bson.BsonWriter;
import org.bson.codecs.Codec;
import org.bson.codecs.DecoderContext;
import org.bson.codecs.EncoderContext;
import org.bson.codecs.configuration.CodecConfigurationException;

import java.lang.reflect.Field;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * @Description:
 * @BelongsProject: mongo
 * @BelongsPackage: com.anwen.mongo.codec
 * @Author: JiaChaoYang
 * @CreateTime: 2023-06-01 21:13
 * @Version: 1.0
 */
public class GenericCodec<T> implements Codec<T> {
    private final Class<T> clazz;

    // 缓存类的属性和类型
    private final Map<String, Field> fieldCache = new ConcurrentHashMap<>();

    // 缓存数值类型的实例
    private final Map<Class<?>, Object> valueCache = new ConcurrentHashMap<>();

    public GenericCodec(Class<T> clazz) {
        this.clazz = clazz;
    }

    @Override
    public void encode(BsonWriter writer, T value, EncoderContext encoderContext) {
        // 将 T 类型的对象转换为 BSON 格式并写入到 writer 中
        writer.writeStartDocument();
        for (Field field : getClassFields()) {
            try {
                field.setAccessible(true);
                // 获取属性值并写入到 writer 中
                Object fieldValue = field.get(value);
                writer.writeName(field.getName());
                writeValue(writer, fieldValue, encoderContext);
            } catch (IllegalAccessException e) {
                throw new CodecConfigurationException(e.getMessage(), e);
            }
        }
        writer.writeEndDocument();
    }

    @Override
    public Class<T> getEncoderClass() {
        return clazz;
    }

    @Override
    public T decode(BsonReader reader, DecoderContext decoderContext) {
        try {
            // 创建 T 类型的实例并赋值
            T instance = clazz.getDeclaredConstructor().newInstance();
            reader.readStartDocument();
            while (reader.readBsonType() != BsonType.END_OF_DOCUMENT) {
                String fieldName = reader.readName();
                // 获取属性对象并设置访问权限
                Field field = getField(fieldName);
                field.setAccessible(true);
                // 获取属性值并设置到实例中
                Object fieldValue = readValue(reader, decoderContext, field.getType());
                field.set(instance, fieldValue);
            }
            reader.readEndDocument();
            return instance;
        } catch (ReflectiveOperationException e) {
            throw new CodecConfigurationException(e.getMessage(), e);
        }
    }

    // 缓存 getClassFields 方法的结果
    private Field[] fields;

    // 使用缓存池获取类的所有字段
    private Field[] getClassFields() {
        if (fields == null) {
            fields = clazz.getDeclaredFields();
        }
        return fields;
    }

    // 使用缓存池获取属性对象
    private Field getField(String name) throws NoSuchFieldException {
        return fieldCache.computeIfAbsent(name, n -> {
            try {
                return clazz.getDeclaredField(n);
            } catch (NoSuchFieldException e) {
                throw new CodecConfigurationException(e.getMessage(), e);
            }
        });
    }

    // 使用缓存池获取数值类型实例
    @SuppressWarnings("unchecked")
    private <V> V getCachedValue(Class<V> type) {
        return (V) valueCache.computeIfAbsent(type, t -> {
            try {
                return t.getDeclaredConstructor().newInstance();
            } catch (ReflectiveOperationException e) {
                throw new CodecConfigurationException(e.getMessage(), e);
            }
        });
    }

    // 将属性值写入到 writer 中
    private void writeValue(BsonWriter writer, Object value, EncoderContext encoderContext) {
        if (value == null) {
            writer.writeNull();
        } else if (value instanceof String) {
            writer.writeString((String) value);
        } else if (value instanceof Boolean) {
            writer.writeBoolean((Boolean) value);
        } else if (value instanceof Integer) {
            writer.writeInt32((Integer) value);
        } else if (value instanceof Long) {
            writer.writeInt64((Long) value);
        } else if (value instanceof Float) {
            writer.writeDouble(((Float) value).doubleValue());
        } else if (value instanceof Double) {
            writer.writeDouble((Double) value);
        } else {
            throw new CodecConfigurationException("Unsupported value type: " + value.getClass().getName());
        }
    }

    // 从 reader 中读取属性值
    private Object readValue(BsonReader reader, DecoderContext decoderContext, Class<?> type) {
        switch (reader.getCurrentBsonType()) {
            case NULL:
                reader.readNull();
                return null;
            case STRING:
                return reader.readString();
            case BOOLEAN:
                return reader.readBoolean();
            case INT32:
                if (type == int.class || type == Integer.class) {
                    return reader.readInt32();
                }
                break;
            case INT64:
                if (type == long.class || type == Long.class) {
                    return reader.readInt64();
                }
                break;
            case DOUBLE:
                if (type == float.class || type == Float.class) {
                    return (float) reader.readDouble();
                }
                if (type == double.class || type == Double.class) {
                    return reader.readDouble();
                }
                break;
            default:
                throw new CodecConfigurationException("Unsupported value type: " + reader.getCurrentBsonType().name());
        }
        // 尝试从缓存池中获取数值类型实例
        return getCachedValue(type);
    }
}
