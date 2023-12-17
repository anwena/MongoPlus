package com.anwen.mongo.toolkit.codec;

import com.anwen.mongo.codec.GenericCodec;
import com.anwen.mongo.toolkit.ClassTypeUtil;
import com.anwen.mongo.toolkit.CollUtil;
import com.mongodb.MongoClientSettings;
import org.bson.codecs.Codec;
import org.bson.codecs.configuration.CodecRegistries;
import org.bson.codecs.configuration.CodecRegistry;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.stream.Collectors;

public class RegisterCodecUtil {

    private static final List<CodecRegistry> codecRegistryList = new CopyOnWriteArrayList<>();

    private static final Map<Class<?>, Codec<?>> codecMap = new ConcurrentHashMap<>();

    static {
        // 静态加载所有默认解码器
        codecRegistryList.add(MongoClientSettings.getDefaultCodecRegistry());
    }

    public static Codec<?> getCodec(Class<?> clazz) {
        return codecMap.get(clazz);
    }

    public static <T> CodecRegistry registerCodec(T t) {
        return registerCodec(ClassTypeUtil.getAllClass(t));
    }

    public static CodecRegistry registerCodec(Map<?, ?> map) {
        if (map == null || map.isEmpty()) {
            // 类map为空，当前已缓存的解码器
            return CodecRegistries.fromRegistries(codecRegistryList);
        }
        return registerCodec(new HashSet<Class<?>>() {{
            map.values().forEach(m -> {
                if (m instanceof List) {
                    System.out.println("找到集合了找到集合了");
                    List<?> list = (List<?>) m;
                    if (CollUtil.isEmpty(list)) {
                        return;
                    }
                    addAll(ClassTypeUtil.getAllClass(list.get(0)));
                } else if (m instanceof Map) {
                    registerCodec((Map<?, ?>) m);
                } else {
                    addAll(ClassTypeUtil.getAllClass(m));
                }
            });
        }});
    }

    public static CodecRegistry registerCodec(Set<Class<?>> fieldClasses) {
        fieldClasses = fieldClasses.stream().filter(clazz -> !codecMap.containsKey(clazz)).collect(Collectors.toSet());
        fieldClasses.parallelStream().forEach(clazz -> {
            Codec<?> codec = new GenericCodec<>(clazz);
            codecMap.put(clazz, codec);
            codecRegistryList.add(CodecRegistries.fromCodecs(codec));
        });
        return CodecRegistries.fromRegistries(codecRegistryList);
    }

    public static CodecRegistry getCodecCacheAndDefault() {
        return CodecRegistries.fromRegistries(codecRegistryList);
    }

}
