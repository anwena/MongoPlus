package com.anwen.mongo.model;

import com.anwen.mongo.toolkit.builder.CompareToBuilder;

import java.io.Serializable;
import java.util.Map;
import java.util.Objects;

/**
 * 由两个元素组成的一对。
 * 这个类是定义基本API的抽象实现。它将元素称为“左”和“右”。它还实现了Map。输入接口，其中键为“left”，值为“right”。
 * 子类实现可以是可变的，也可以是不可变的。然而，对可以存储的存储对象的类型没有限制。如果可变对象存储在对中，那么对本身实际上就是可变的。
 * @author JiaChaoYang
 **/
public abstract class Pair<L , R> implements Map.Entry<L, R>, Comparable<Pair<L, R>>, Serializable {


    private static final long serialVersionUID = 5257775966081801053L;

    /**
     * 从两个推断泛型类型的对象中获得一对不可变的。
     * 这个工厂允许使用推理来创建对，以获得泛型类型
     * @author JiaChaoYang
     * @date 2024/3/16 22:48
    */
    public static <L, R> Pair<L, R> of(final L left, final R right) {
        return new MutablePair<>(left, right);
    }

    /**
     * 从该对中获取左侧元素
     * @author JiaChaoYang
     * @date 2024/3/16 22:47
    */
    public abstract L getLeft();

    /**
     * 从这对中获取右侧元素
     * @author JiaChaoYang
     * @date 2024/3/16 22:47
    */
    public abstract R getRight();

    /**
     * 从这对中获取KEY。
     * @author JiaChaoYang
     * @date 2024/3/16 22:47
    */
    @Override
    public final L getKey() {
        return getLeft();
    }

    /**
     * 从该对中获取值
     * @author JiaChaoYang
     * @date 2024/3/16 22:47
    */
    @Override
    public R getValue() {
        return getRight();
    }

    /**
     * 比较基于左元素和右元素的对。类型必须是可比较的。
     * @author JiaChaoYang
     * @date 2024/3/16 22:47
    */
    @Override
    public int compareTo(final Pair<L, R> other) {
        return new CompareToBuilder().append(getLeft(), other.getLeft())
                .append(getRight(), other.getRight()).toComparison();
    }

    /**
     * 根据这两个元素将此对与另一对进行比较。
     * @author JiaChaoYang
     * @date 2024/3/16 22:46
    */
    @Override
    public boolean equals(final Object obj) {
        if (obj == this) {
            return true;
        }
        if (obj instanceof Map.Entry<?, ?>) {
            final Map.Entry<?, ?> other = (Map.Entry<?, ?>) obj;
            return Objects.equals(getKey(), other.getKey())
                    && Objects.equals(getValue(), other.getValue());
        }
        return false;
    }

    /**
     * 返回合适的哈希代码。哈希代码遵循Map中的定义
     * @author JiaChaoYang
     * @date 2024/3/16 22:52
    */
    @Override
    public int hashCode() {
        // see Map.Entry API specification
        return (getKey() == null ? 0 : getKey().hashCode()) ^
                (getValue() == null ? 0 : getValue().hashCode());
    }

    /**
     * 使用格式（$left，$right）返回此对的字符串表示形式
     * @author JiaChaoYang
     * @date 2024/3/16 22:52
    */
    @Override
    public String toString() {
        return new StringBuilder().append('(').append(getLeft()).append(',').append(getRight()).append(')').toString();
    }

    /**
     * 使用给定的格式格式化接收器。
     * 这使用java.util。Formattable以执行格式化。可以使用两个变量来嵌入左元素和右元素。将%1$s用于左侧元素（键），将%2$s用于右侧元素（值）。toString（）使用的默认格式是（%1$s，%2$s）。
     * @author JiaChaoYang
     * @date 2024/3/16 22:52
    */
    public String toString(final String format) {
        return String.format(format, getLeft(), getRight());
    }
}
