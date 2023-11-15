package com.anwen.mongo.model;

import com.anwen.mongo.support.SFunction;
import org.bson.BsonDocument;
import org.bson.BsonInt32;
import org.bson.BsonString;
import org.bson.BsonValue;
import org.bson.codecs.configuration.CodecRegistry;
import org.bson.conversions.Bson;

import java.util.List;
import java.util.stream.Collectors;

import static com.mongodb.assertions.Assertions.notNull;
import static java.util.Arrays.asList;

/**
 * @author JiaChaoYang
 * @project mongo-plus
 * @description 用于定义索引键的工厂。使用此类的一种便捷方法是静态导入其所有方法，这允许使用如下方法: collection.createIndex(compoundIndex(ascending("x"), descending("y")));
 * @since 引用自 {@link com.mongodb.client.model.Indexes}将其扩展，支持函数
 * @date 2023-11-15 14:17
 **/
public final class Indexes {

    private Indexes() {
    }

    /**
     * 为给定字段的升序索引创建索引键。
     *
     * @param fieldNames 字段名称，必须至少包含一个
     * @return 索引详述
     */
    public static Bson ascending(final String... fieldNames) {
        return ascending(asList(fieldNames));
    }

    /**
     * 为给定字段的升序索引创建索引键。
     *
     * @param fieldNames 字段名称，必须至少包含一个
     * @return 索引详述
     */
    @SafeVarargs
    public static <T> Bson ascendingFunc(final SFunction<T,Object>... fieldNames) {
        return ascendingFunc(asList(fieldNames));
    }

    /**
     * 为给定字段的升序索引创建索引键。
     *
     * @param fieldNames 字段名称，必须至少包含一个
     * @return 索引详述
     */
    public static Bson ascending(final List<String> fieldNames) {
        notNull("fieldNames", fieldNames);
        return compoundIndex(fieldNames, new BsonInt32(1));
    }

    /**
     * 为给定字段的升序索引创建索引键。
     *
     * @param fieldNames 字段名称，必须至少包含一个
     * @return 索引详述
     */
    public static <T> Bson ascendingFunc(final List<SFunction<T,Object>> fieldNames) {
        notNull("fieldNames", fieldNames);
        return compoundIndex(fieldNames.stream().map(SFunction::getFieldNameLine).collect(Collectors.toList()), new BsonInt32(1));
    }

    /**
     * 为给定字段的降序索引创建索引键。
     *
     * @param fieldNames 字段名称，必须至少包含一个
     * @return 索引详述
     */
    public static Bson descending(final String... fieldNames) {
        return descending(asList(fieldNames));
    }

    /**
     * 为给定字段的降序索引创建索引键。
     *
     * @param fieldNames 字段名称，必须至少包含一个
     * @return 索引详述
     */
    @SafeVarargs
    public static <T> Bson descendingFunc(final SFunction<T,Object>... fieldNames) {
        return descendingFunc(asList(fieldNames));
    }

    /**
     * 为给定字段的降序索引创建索引键。
     *
     * @param fieldNames 字段名称，必须至少包含一个
     * @return 索引详述
     */
    public static Bson descending(final List<String> fieldNames) {
        notNull("fieldNames", fieldNames);
        return compoundIndex(fieldNames, new BsonInt32(-1));
    }

    /**
     * 为给定字段的降序索引创建索引键。
     *
     * @param fieldNames 字段名称，必须至少包含一个
     * @return 索引详述
     */
    public static <T> Bson descendingFunc(final List<SFunction<T,Object>> fieldNames) {
        notNull("fieldNames", fieldNames);
        return compoundIndex(fieldNames.stream().map(SFunction::getFieldNameLine).collect(Collectors.toList()), new BsonInt32(-1));
    }

    /**
     * 在给定字段上为 2dsphere 索引创建索引键。
     *
     * @param fieldNames 字段名称，必须至少包含一个
     * @return 索引详述
     */
    public static Bson geo2dsphere(final String... fieldNames) {
        return geo2dsphere(asList(fieldNames));
    }

    /**
     * 在给定字段上为 2dsphere 索引创建索引键。
     *
     * @param fieldNames 字段名称，必须至少包含一个
     * @return 索引详述
     */
    public static <T> Bson geo2dsphereFunc(final SFunction<T,Object>... fieldNames) {
        return geo2dsphereFunc(asList(fieldNames));
    }

    /**
     * 在给定字段上为 2dsphere 索引创建索引键。
     *
     * @param fieldNames 字段名称，必须至少包含一个
     * @return 索引详述
     */
    public static Bson geo2dsphere(final List<String> fieldNames) {
        notNull("fieldNames", fieldNames);
        return compoundIndex(fieldNames, new BsonString("2dsphere"));
    }

    /**
     * 在给定字段上为 2dsphere 索引创建索引键。
     *
     * @param fieldNames 字段名称，必须至少包含一个
     * @return 索引详述
     */
    public static <T> Bson geo2dsphereFunc(final List<SFunction<T,Object>> fieldNames) {
        notNull("fieldNames", fieldNames);
        return compoundIndex(fieldNames.stream().map(SFunction::getFieldNameLine).collect(Collectors.toList()), new BsonString("2dsphere"));
    }

    /**
     * 在给定字段上为 2d 索引创建索引键。
     *
     * <p>
     * <strong>注意：</strong>二维索引用于在二维平面上存储为点的数据。
     * 2d 索引用于 MongoDB 2.2 及更早版本中使用的旧坐标对。
     * </p>
     *
     * @param fieldName 要在其上创建 2D 索引的字段
     * @return 索引详述
     */
    public static Bson geo2d(final String fieldName) {
        notNull("fieldName", fieldName);
        return new BsonDocument(fieldName, new BsonString("2d"));
    }

    /**
     * 在给定字段上为 2d 索引创建索引键。
     *
     * <p>
     * <strong>注意：</strong>二维索引用于在二维平面上存储为点的数据。
     * 2d 索引用于 MongoDB 2.2 及更早版本中使用的旧坐标对。
     * </p>
     *
     * @param fieldName 要在其上创建 2D 索引的字段
     * @return 索引详述
     */
    public static <T> Bson geo2d(final SFunction<T,Object> fieldName) {
        notNull("fieldName", fieldName);
        return new BsonDocument(fieldName.getFieldNameLine(), new BsonString("2d"));
    }

    /**
     * 在给定字段上为 geohaystack 索引创建索引键。
     *
     * <p>
     * <strong>注意：</strong>对于使用球形几何图形的查询，2dsphere 索引是比大海捞针索引更好的选择。
     * 2dsphere 索引允许字段重新排序;geoHaystack 索引要求第一个字段是位置字段。此外，geoHaystack
     * 索引只能通过命令使用，因此始终一次返回所有结果。
     * </p>
     *
     * @param fieldName 要在其上创建 geoHaystack 索引的字段
     * @param additional 构成 geoHaystack 索引键的附加字段
     * @return 索引详述
     * @deprecated geoHaystack 在 MongoDB 4.4 中已弃用，首选 {@link com.anwen.mongo.model.Indexes#geo2dsphere(String...)}
     */
    @Deprecated
    public static Bson geoHaystack(final String fieldName, final Bson additional) {
        notNull("fieldName", fieldName);
        return compoundIndex(new BsonDocument(fieldName, new BsonString("geoHaystack")), additional);
    }

    /**
     * 在给定字段上为 geohaystack 索引创建索引键。
     *
     * <p>
     * <strong>注意：</strong>对于使用球形几何图形的查询，2dsphere 索引是比大海捞针索引更好的选择。
     * 2dsphere 索引允许字段重新排序;geoHaystack 索引要求第一个字段是位置字段。此外，geoHaystack
     * 索引只能通过命令使用，因此始终一次返回所有结果。
     * </p>
     *
     * @param fieldName 要在其上创建 geoHaystack 索引的字段
     * @param additional 构成 geoHaystack 索引键的附加字段
     * @return 索引详述
     * @deprecated geoHaystack 在 MongoDB 4.4 中已弃用，首选 {@link com.anwen.mongo.model.Indexes#geo2dsphere(String...)}
     */
    @Deprecated
    public static <T> Bson geoHaystack(final SFunction<T,Object> fieldName, final Bson additional) {
        notNull("fieldName", fieldName);
        return compoundIndex(new BsonDocument(fieldName.getFieldNameLine(), new BsonString("geoHaystack")), additional);
    }

    /**
     * 为给定字段上的文本索引创建索引键。
     *
     * @param fieldName 要在其上创建文本索引的字段
     * @return 索引详述
     */
    public static Bson text(final String fieldName) {
        notNull("fieldName", fieldName);
        return new BsonDocument(fieldName, new BsonString("text"));
    }

    /**
     * 为给定字段上的文本索引创建索引键。
     *
     * @param fieldName 要在其上创建文本索引的字段
     * @return 索引详述
     */
    public static <T> Bson text(final SFunction<T,Object> fieldName) {
        notNull("fieldName", fieldName);
        return new BsonDocument(fieldName.getFieldNameLine(), new BsonString("text"));
    }

    /**
     * 为包含字符串数据的每个字段上的文本索引创建索引键。
     *
     * @return 索引详述
     */
    public static Bson text() {
        return text("$**");
    }

    /**
     * 为给定字段上的哈希索引创建索引键。
     *
     * @param fieldName 要在其上创建哈希索引的字段
     * @return 索引详述
     */
    public static Bson hashed(final String fieldName) {
        notNull("fieldName", fieldName);
        return new BsonDocument(fieldName, new BsonString("hashed"));
    }

    /**
     * 为给定字段上的哈希索引创建索引键。
     *
     * @param fieldName 要在其上创建哈希索引的字段
     * @return 索引详述
     */
    public static <T> Bson hashed(final SFunction<T,Object> fieldName) {
        notNull("fieldName", fieldName);
        return new BsonDocument(fieldName.getFieldNameLine(), new BsonString("hashed"));
    }

    /**
     * 创建复合索引详述。 如果任何字段名称重复，则最后一个字段名称优先。
     *
     * @param indexes 索引规格
     * @return 复合指数规格
     */
    public static Bson compoundIndex(final Bson... indexes) {
        return compoundIndex(asList(indexes));
    }

    /**
     * 复合多指标规格。 如果任何字段名称重复，则最后一个字段名称优先。
     *
     * @param indexes 索引规格
     * @return 复合指数规格
     */
    public static Bson compoundIndex(final List<? extends Bson> indexes) {
        return new CompoundIndex(indexes);
    }

    private static Bson compoundIndex(final List<String> fieldNames, final BsonValue value) {
        BsonDocument document = new BsonDocument();
        for (String fieldName : fieldNames) {
            document.append(fieldName, value);
        }
        return document;
    }

    private static class CompoundIndex implements Bson {
        private final List<? extends Bson> indexes;

        CompoundIndex(final List<? extends Bson> indexes) {
            notNull("indexes", indexes);
            this.indexes = indexes;
        }

        @Override
        public <TDocument> BsonDocument toBsonDocument(final Class<TDocument> documentClass, final CodecRegistry codecRegistry) {
            BsonDocument compoundIndex = new BsonDocument();
            for (Bson index : indexes) {
                BsonDocument indexDocument = index.toBsonDocument(documentClass, codecRegistry);
                for (String key : indexDocument.keySet()) {
                    compoundIndex.append(key, indexDocument.get(key));
                }
            }
            return compoundIndex;
        }

        @Override
        public boolean equals(final Object o) {
            if (this == o) {
                return true;
            }
            if (o == null || getClass() != o.getClass()) {
                return false;
            }

            CompoundIndex that = (CompoundIndex) o;

            return indexes.equals(that.indexes);
        }

        @Override
        public int hashCode() {
            return indexes.hashCode();
        }
    }
}
