package com.anwen.mongo.conditions.interfaces.aggregate.pipeline;

import com.anwen.mongo.conditions.interfaces.condition.Order;
import com.anwen.mongo.support.SFunction;
import com.mongodb.client.model.Filters;
import com.mongodb.client.model.TextSearchOptions;
import org.bson.BsonDocument;
import org.bson.BsonInt32;
import org.bson.BsonString;
import org.bson.BsonValue;
import org.bson.codecs.configuration.CodecRegistry;
import org.bson.conversions.Bson;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import static com.mongodb.assertions.Assertions.notNull;
import static java.util.Arrays.asList;

/**
 * 排序
 *
 * @author anwen
 * @date 2024/6/15 上午3:00
 */
public class Sorts {

    /**
     * 升序排序
     *
     * @param fieldNames 字段名称，必须至少包含一个
     * @return 排序规范
     * @since mongodb.driver.manual reference/operator/meta/orderby Sort
     */
    public static Bson asc(final String... fieldNames) {
        return asc(asList(fieldNames));
    }

    /**
     * 升序排序
     *
     * @param fieldNames 字段名称，必须至少包含一个
     * @return 排序规范
     * @since mongodb.driver.manual reference/operator/meta/orderby Sort
     */
    public static <T> Bson asc(final SFunction<T,?>... fieldNames) {
        return asc(Arrays.stream(fieldNames).map(SFunction::getFieldNameLine).toArray(String[]::new));
    }

    /**
     * 升序排序
     *
     * @param fieldNames 字段名称，必须至少包含一个
     * @return 排序规范
     * @since mongodb.driver.manual reference/operator/meta/orderby Sort
     */
    public static <T> Bson ascLambda(final List<SFunction<T,?>> fieldNames) {
        return asc(fieldNames.stream().map(SFunction::getFieldNameLine).collect(Collectors.toList()));
    }

    /**
     * 升序排序
     *
     * @param fieldNames 字段名称，必须至少包含一个
     * @return 排序规范
     * @since mongodb.driver.manual reference/operator/meta/orderby Sort
     */
    public static Bson asc(final List<String> fieldNames) {
        notNull("fieldNames", fieldNames);
        return orderBy(fieldNames, new BsonInt32(1));
    }

    /**
     * 降序排序
     *
     * @param fieldNames 字段名称，必须至少包含一个
     * @return 排序规范
     * @since mongodb.driver.manual reference/operator/meta/orderby Sort
     */
    public static Bson desc(final String... fieldNames) {
        return desc(asList(fieldNames));
    }

    /**
     * 降序排序
     *
     * @param fieldNames 字段名称，必须至少包含一个
     * @return 排序规范
     * @since mongodb.driver.manual reference/operator/meta/orderby Sort
     */
    public static <T> Bson desc(final SFunction<T,?>... fieldNames) {
        return desc(Arrays.stream(fieldNames).map(SFunction::getFieldNameLine).collect(Collectors.toList()));
    }

    /**
     * 降序排序
     *
     * @param fieldNames 字段名称，必须至少包含一个
     * @return 排序规范
     * @since mongodb.driver.manual reference/operator/meta/orderby Sort
     */
    public static <T> Bson descLambda(final List<SFunction<T,?>> fieldNames) {
        return desc(fieldNames.stream().map(SFunction::getFieldNameLine).collect(Collectors.toList()));
    }

    /**
     * 降序排序
     *
     * @param fieldNames 字段名称，必须至少包含一个
     * @return 排序规范
     * @since mongodb.driver.manual reference/operator/meta/orderby Sort
     */
    public static Bson desc(final List<String> fieldNames) {
        notNull("fieldNames", fieldNames);
        return orderBy(fieldNames, new BsonInt32(-1));
    }

    /**
     * 为给定字段上的文本分数元投影创建排序规范.
     *
     * @param fieldName 字段名称
     * @return 排序规范
     * @see Filters#text(String, TextSearchOptions)
     * @since mongodb.driver.manual reference/operator/aggregation/meta/#text-score-metadata--meta---textscore- textScore
     */
    public static Bson metaTextScore(final String fieldName) {
        return new BsonDocument(fieldName, new BsonDocument("$meta", new BsonString("textScore")));
    }

    /**
     * 为给定字段上的文本分数元投影创建排序规范.
     *
     * @param fieldName 字段名称
     * @return 排序规范
     * @see Filters#text(String, TextSearchOptions)
     * @since mongodb.driver.manual reference/operator/aggregation/meta/#text-score-metadata--meta---textscore- textScore
     */
    public static <T> Bson metaTextScore(final SFunction<T,?> fieldName) {
        return metaTextScore(fieldName.getFieldNameLine());
    }

    /**
     * 组合多个排序规范。如果任何字段名称重复，则最后一个字段优先.
     *
     * @param sorts 排序规格
     * @return 组合排序规范
     */
    public static Bson orderBy(final Bson... sorts) {
        return orderBy(asList(sorts));
    }

    /**
     * 组合多个排序规范。如果任何字段名称重复，则最后一个字段优先.
     *
     * @param orders 排序规范
     * @return 组合排序规范
     */
    public static Bson orderBy(final Order... orders) {
        return orderBy(Arrays.stream(orders).map(order -> new BsonDocument(order.getColumn(), new BsonInt32(order.getType()))).collect(Collectors.toList()));
    }

    /**
     * 组合多个排序规范。如果任何字段名称重复，则最后一个字段优先
     *
     * @param sorts 排序规范
     * @return 组合排序规范
     */
    public static Bson orderBy(final List<? extends Bson> sorts) {
        notNull("sorts", sorts);
        return new CompoundSort(sorts);
    }

    private static Bson orderBy(final List<String> fieldNames, final BsonValue value) {
        BsonDocument document = new BsonDocument();
        for (String fieldName : fieldNames) {
            document.append(fieldName, value);
        }
        return document;
    }

    private static final class CompoundSort implements Bson {
        private final List<? extends Bson> sorts;

        private CompoundSort(final List<? extends Bson> sorts) {
            this.sorts = sorts;
        }

        @Override
        public <TDocument> BsonDocument toBsonDocument(final Class<TDocument> documentClass, final CodecRegistry codecRegistry) {
            BsonDocument combinedDocument = new BsonDocument();
            for (Bson sort : sorts) {
                BsonDocument sortDocument = sort.toBsonDocument(documentClass, codecRegistry);
                for (String key : sortDocument.keySet()) {
                    combinedDocument.append(key, sortDocument.get(key));
                }
            }
            return combinedDocument;
        }

        @Override
        public boolean equals(final Object o) {
            if (this == o) {
                return true;
            }
            if (o == null || getClass() != o.getClass()) {
                return false;
            }

            CompoundSort that = (CompoundSort) o;

            return Objects.equals(sorts, that.sorts);
        }

        @Override
        public int hashCode() {
            return sorts != null ? sorts.hashCode() : 0;
        }

        @Override
        public String toString() {
            return "Compound Sort{"
                    + "sorts=" + sorts
                    + '}';
        }
    }
    
}
