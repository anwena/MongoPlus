package com.anwen.mongo.mapping;

import com.anwen.mongo.toolkit.Assert;

import java.util.*;

/**
 * 简单容器，用于容纳一组被视为简单类型的类型
 * @author JiaChaoYang
 * @since Spring
 **/
public class SimpleTypeHolder {

    private static final Set<Class<?>> DEFAULTS;
    static {
        Set<Class<?>> defaults = new HashSet<>();
        defaults.add(boolean.class);
        defaults.add(boolean[].class);
        defaults.add(long.class);
        defaults.add(long[].class);
        defaults.add(short.class);
        defaults.add(short[].class);
        defaults.add(int.class);
        defaults.add(int[].class);
        defaults.add(byte.class);
        defaults.add(byte[].class);
        defaults.add(float.class);
        defaults.add(float[].class);
        defaults.add(double.class);
        defaults.add(double[].class);
        defaults.add(char.class);
        defaults.add(char[].class);
        defaults.add(Boolean.class);
        defaults.add(Long.class);
        defaults.add(Short.class);
        defaults.add(Integer.class);
        defaults.add(Byte.class);
        defaults.add(Float.class);
        defaults.add(Double.class);
        defaults.add(Character.class);
        defaults.add(String.class);
        defaults.add(Date.class);
        defaults.add(Locale.class);
        defaults.add(Class.class);
        defaults.add(Enum.class);
        DEFAULTS = Collections.unmodifiableSet(defaults);
    }

    public static final SimpleTypeHolder DEFAULT = new SimpleTypeHolder();

    private volatile Map<Class<?>, Boolean> simpleTypes;

    /**
     * 创建一个包含默认类型的新｛@link SimpleTypeHolder｝。
     *
     * @see #SimpleTypeHolder(Set, boolean)
     */
    public SimpleTypeHolder() {
        this(Collections.emptySet(), true);
    }

    /**
     * 创建一个新的{@link SimpleTypeHolder}来承载给定的自定义简单类型。注册默认简单类型
     * 可以通过为{@code registerDefaults}传递{@literal false}来停用。
     *
     * @param customSimpleTypes 自定义简单类型
     * @param registerDefaults 是否注册默认类型为简单类型
     */
    public SimpleTypeHolder(Set<? extends Class<?>> customSimpleTypes, boolean registerDefaults) {

        Assert.notNull(customSimpleTypes, "CustomSimpleTypes must not be null");

        this.simpleTypes = new WeakHashMap<>(customSimpleTypes.size() + DEFAULTS.size());

        register(customSimpleTypes);

        if (registerDefaults) {
            register(DEFAULTS);
        }
    }

    /**
     * 复制构造函数以创建一个新的{@link SimpleTypeHolder}，它承载给定的其他自定义简单类型。
     *
     * @param customSimpleTypes 不得为{@literal null}
     * @param source 不得为{@literal null}
     */
    public SimpleTypeHolder(Set<? extends Class<?>> customSimpleTypes, SimpleTypeHolder source) {

        Assert.notNull(customSimpleTypes, "CustomSimpleTypes must not be null");
        Assert.notNull(source, "SourceTypeHolder must not be null");

        this.simpleTypes = new WeakHashMap<>(customSimpleTypes.size() + source.simpleTypes.size());

        register(customSimpleTypes);
        registerCachePositives(source.simpleTypes);
    }

    private void registerCachePositives(Map<Class<?>, Boolean> source) {

        for (Map.Entry<Class<?>, Boolean> entry : source.entrySet()) {

            if (!entry.getValue()) {
                continue;
            }

            this.simpleTypes.put(entry.getKey(), true);
        }
    }

    /**
     * 返回给定类型是否被视为简单类型。
     *
     * @param type 不得为{@literal null}。
     * @return
     */
    public boolean isSimpleType(Class<?> type) {

        Assert.notNull(type, "Type must not be null");

        Map<Class<?>, Boolean> localSimpleTypes = this.simpleTypes;
        Boolean isSimpleType = localSimpleTypes.get(type);

        if (Object.class.equals(type) || Enum.class.isAssignableFrom(type)) {
            return true;
        }

        if (isSimpleType != null) {
            return isSimpleType;
        }

        String typeName = type.getName();

        if (typeName.startsWith("java.lang") || type.getName().startsWith("java.time") || typeName.equals("kotlin.Unit")) {
            return true;
        }

        for (Class<?> simpleType : localSimpleTypes.keySet()) {

            if (simpleType.isAssignableFrom(type)) {

                isSimpleType = localSimpleTypes.get(simpleType);
                this.simpleTypes = put(localSimpleTypes, type, isSimpleType);
                return isSimpleType;
            }
        }

        this.simpleTypes = put(localSimpleTypes, type, false);

        return false;
    }

    private void register(Collection<? extends Class<?>> types) {
        types.forEach(customSimpleType -> this.simpleTypes.put(customSimpleType, true));
    }

    private static Map<Class<?>, Boolean> put(Map<Class<?>, Boolean> simpleTypes, Class<?> type, boolean isSimpleType) {

        Map<Class<?>, Boolean> copy = new WeakHashMap<>(simpleTypes);
        copy.put(type, isSimpleType);

        return copy;
    }

}
