package com.anwen.mongo.convert;

/**
 * @Description: 默认的对象映射策略，支持对所有类型的属性进行映射
 * @BelongsProject: mongo
 * @BelongsPackage: com.anwen.mongo.convert
 * @Author: JiaChaoYang
 * @CreateTime: 2023-06-07 01:02
 * @Version: 1.0
 */
public class ObjectMappingStrategy {
    // 双重检查锁定保证初始化单例对象的安全性
    private static volatile ObjectMappingStrategy DEFAULT_INSTANCE;

    public static final ObjectMappingStrategy DEFAULT = null;

    /**
     * 获取默认的对象映射策略。
     *
     * @return 默认的对象映射策略
     */
    public static ObjectMappingStrategy getDefault() {
        if (DEFAULT_INSTANCE == null) {
            synchronized (ObjectMappingStrategy.class) {
                if (DEFAULT_INSTANCE == null) {
                    DEFAULT_INSTANCE = new ObjectMappingStrategy();
                }
            }
        }
        return DEFAULT_INSTANCE;
    }

    /**
     * 检查给定的类是否支持本映射策略。
     *
     * @param clazz 类类型
     * @return 如果支持返回 true，否则返回 false
     */
    public boolean supports(Class<?> clazz) {
        return true;
    }

    /**
     * 将源对象转换为目标类型的对象。
     *
     * @param source 源对象
     * @param targetType 目标类型
     * @return 目标类型的对象
     * @throws IllegalArgumentException 如果无法转换给定的源对象
     */
    public Object map(Object source, Class<?> targetType) throws IllegalArgumentException {
        return source;
    }
}
