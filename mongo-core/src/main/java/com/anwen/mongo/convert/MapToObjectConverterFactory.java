package com.anwen.mongo.convert;

/**
 * @Description: MapToObjectConverter 工厂类，用于创建 MapToObjectConverter 实例
 * @BelongsProject: mongo
 * @BelongsPackage: com.anwen.mongo.convert
 * @Author: JiaChaoYang
 * @CreateTime: 2023-06-07 01:01
 * @Version: 1.0
 */
public class MapToObjectConverterFactory {
    private MapToObjectConverterFactory() {}

    /**
     * 使用默认配置创建一个 MapToObjectConverter 实例。
     *
     * @param clazz 类类型
     * @return MapToObjectConverter 实例
     */
    public static <T> MapToObjectConverter<T> createConverter(Class<T> clazz) {
        return createConverter(clazz, ObjectMappingStrategy.DEFAULT);
    }

    /**
     * 根据类类型和映射策略创建相应的 MapToObjectConverter 实例。
     *
     * @param clazz 类类型
     * @param strategy 映射策略
     * @return MapToObjectConverter 实例
     * @throws IllegalArgumentException 如果不支持给定的类类型或映射策略
     */
    public static <T> MapToObjectConverter<T> createConverter(Class<T> clazz, ObjectMappingStrategy strategy)
            throws IllegalArgumentException {
        if (clazz == null) {
            throw new IllegalArgumentException("Class type should not be null");
        }
        if (!strategy.supports(clazz)) {
            throw new IllegalArgumentException("Unsupported object mapping strategy for class: " + clazz.getName());
        }
        return new MapToObjectConverter<T>(clazz, strategy);
    }

}
