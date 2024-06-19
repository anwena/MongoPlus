package com.anwen.mongo.conditions.interfaces.aggregate.pipeline.project;

import com.anwen.mongo.conditions.BuildCondition;
import com.anwen.mongo.conditions.query.QueryChainWrapper;
import com.anwen.mongo.support.SFunction;
import com.mongodb.client.model.Aggregates;
import com.mongodb.client.model.Filters;
import com.mongodb.client.model.TextSearchOptions;
import com.mongodb.client.model.search.SearchCollector;
import com.mongodb.client.model.search.SearchCount;
import com.mongodb.client.model.search.SearchOperator;
import com.mongodb.client.model.search.SearchOptions;
import org.bson.*;
import org.bson.codecs.configuration.CodecRegistry;
import org.bson.conversions.Bson;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import static com.mongodb.assertions.Assertions.notNull;
import static java.util.Arrays.asList;

/**
 * 构建Project阶段
 *
 * @author anwen
 * @date 2024/6/11 上午12:36
 */
public class Projections {

    /**
     * 创建字段的投影，其值是根据给定表达式计算的。仅支持使用$project聚合管道阶段进行带有表达式的投影
     *
     * @param fieldName     字段名
     * @param expression    表达式
     * @param <TExpression> 表达式类型
     * @return $project
     * @see #computedSearchMeta(String)
     * @see Aggregates#project(Bson)
     */
    public static <TExpression> Bson computed(final String fieldName, final TExpression expression) {
        return new SimpleExpression<>(fieldName, expression);
    }

    /**
     * 创建字段的投影，其值是根据给定表达式计算的。仅支持使用$project聚合管道阶段进行带有表达式的投影
     *
     * @param fieldName     字段名
     * @param expression    表达式
     * @param <TExpression> 表达式类型
     * @return $project
     * @see #computedSearchMeta(String)
     * @see Aggregates#project(Bson)
     */
    public static <TExpression,T> Bson computed(final SFunction<T,?> fieldName, final TExpression expression) {
        return computed(fieldName.getFieldNameLine(), expression);
    }

    /**
     * 创建值等于 {@code $$SEARCH_META} 变量的字段的投影，
     * 用于 {@link Aggregates#search(SearchOperator, SearchOptions)} {@link Aggregates#search(SearchCollector, SearchOptions)}。
     * 调用此方法等效于调用 {@link #computed(String, Object)} 并使用 {@code $$SEARCH_META} 作为第二个参数。
     *
     * @param fieldName 字段名
     * @return $project
     * @see SearchCount
     * @see SearchCollector
     * @since 4.7
     */
    public static Bson computedSearchMeta(final String fieldName) {
        return computed(fieldName, "$$SEARCH_META");
    }

    /**
     * 创建值等于 {@code $$SEARCH_META} 变量的字段的投影，
     * 用于 {@link Aggregates#search(SearchOperator, SearchOptions)} {@link Aggregates#search(SearchCollector, SearchOptions)}。
     * 调用此方法等效于调用 {@link #computed(String, Object)} 并使用 {@code $$SEARCH_META} 作为第二个参数。
     *
     * @param fieldName 字段名
     * @return $project
     * @see SearchCount
     * @see SearchCollector
     * @since 4.7
     */
    public static <T> Bson computedSearchMeta(final SFunction<T,?> fieldName) {
        return computed(fieldName, "$$SEARCH_META");
    }

    /**
     * 创建包含所有给定字段的投影
     *
     * @param fieldNames 字段名
     * @return $project
     */
    public static Bson include(final String... fieldNames) {
        return include(asList(fieldNames));
    }

    /**
     * 创建包含所有给定字段的投影
     *
     * @param fieldNames 字段名
     * @return $project
     */
    @SafeVarargs
    public static <T> Bson include(final SFunction<T,?>... fieldNames) {
        return include(Arrays.stream(fieldNames).map(SFunction::getFieldNameLine).toArray(String[]::new));
    }

    /**
     * 创建包含所有给定字段的投影
     *
     * @param fieldNames 字段名
     * @return $project
     */
    public static Bson include(final List<String> fieldNames) {
        return combine(fieldNames, new BsonInt32(1));
    }

    /**
     * 创建包含所有给定字段的投影
     *
     * @param fieldNames 字段名
     * @return $project
     */
    public static <T> Bson includeLambda(final List<SFunction<T,?>> fieldNames) {
        return combine(fieldNames.stream().map(SFunction::getFieldNameLine).collect(Collectors.toList()), new BsonInt32(1));
    }

    /**
     * 创建排除所有给定字段的投影
     *
     * @param fieldNames 字段名
     * @return $project
     */
    public static Bson exclude(final String... fieldNames) {
        return exclude(asList(fieldNames));
    }

    /**
     * 创建排除所有给定字段的投影
     *
     * @param fieldNames 字段名
     * @return $project
     */
    @SafeVarargs
    public static <T> Bson exclude(final SFunction<T,?>... fieldNames) {
        return exclude(Arrays.stream(fieldNames).map(SFunction::getFieldNameLine).toArray(String[]::new));
    }

    /**
     * 创建排除所有给定字段的投影
     *
     * @param fieldNames 字段名
     * @return $project
     */
    public static Bson exclude(final List<String> fieldNames) {
        return combine(fieldNames, new BsonInt32(0));
    }

    /**
     * 创建排除所有给定字段的投影
     *
     * @param fieldNames 字段名
     * @return $project
     */
    public static <T> Bson excludeLambda(final List<SFunction<T,?>> fieldNames) {
        return combine(fieldNames.stream().map(SFunction::getFieldNameLine).collect(Collectors.toList()), new BsonInt32(0));
    }

    /**
     * 创建一个排除 _id 字段的投影。这将禁止自动包含默认的 _id，即使明确包含其他字段也是如此
     *
     * @return $project
     */
    public static Bson excludeId() {
        return new BsonDocument("_id", new BsonInt32(0));
    }

    /**
     * 创建一个投影，其中仅包含与查询过滤器匹配的数组的第一个元素（对于给定字段）。这称为位置 $ 运算符。
     *
     * @param fieldName 值为数组的字段名称
     * @return $project
     * @since mongodb.driver.manual reference/operator/projection/positional/#projection Project the first matching element ($ operator)
     */
    public static Bson elemMatch(final String fieldName) {
        return new BsonDocument(fieldName + ".$", new BsonInt32(1));
    }

    /**
     * 创建一个投影，其中仅包含与查询过滤器匹配的数组的第一个元素（对于给定字段）。这称为位置 $ 运算符。
     *
     * @param fieldName 值为数组的字段名称
     * @return $project
     * @since mongodb.driver.manual reference/operator/projection/positional/#projection Project the first matching element ($ operator)
     */
    public static <T> Bson elemMatch(final SFunction<T,?> fieldName) {
        return elemMatch(fieldName.getFieldNameLine());
    }

    /**
     * 创建一个投影，其中仅包含给定字段的与给定查询过滤器匹配的该字段的数组值的第一个元素。
     *
     * @param fieldName 字段名称
     * @param filter    要应用的过滤器
     * @return $project
     * @since mongodb.driver.manual reference/operator/projection/elemMatch elemMatch
     */
    public static Bson elemMatch(final String fieldName, final Bson filter) {
        return new Projections.ElemMatchFilterProjection(fieldName, filter);
    }

    /**
     * 创建一个投影，其中仅包含给定字段的与给定查询过滤器匹配的该字段的数组值的第一个元素。
     *
     * @param fieldName 字段名称
     * @param queryChainWrapper    条件构造器
     * @return $project
     * @since mongodb.driver.manual reference/operator/projection/elemMatch elemMatch
     */
    public static Bson elemMatch(final String fieldName, final QueryChainWrapper<?, ?> queryChainWrapper) {
        return elemMatch(fieldName, BuildCondition.buildQueryCondition(queryChainWrapper.getCompareList()));
    }

    /**
     * 创建一个投影，其中仅包含给定字段的与给定查询过滤器匹配的该字段的数组值的第一个元素。
     *
     * @param fieldName 字段名称
     * @param filter    要应用的过滤器
     * @return $project
     * @since mongodb.driver.manual reference/operator/projection/elemMatch elemMatch
     */
    public static <T> Bson elemMatch(final SFunction<T,?> fieldName, final Bson filter) {
        return elemMatch(fieldName.getFieldNameLine(),filter);
    }

    /**
     * 创建一个投影，其中仅包含给定字段的与给定查询过滤器匹配的该字段的数组值的第一个元素。
     *
     * @param fieldName 字段名称
     * @param queryChainWrapper    条件构造器
     * @return $project
     * @since mongodb.driver.manual reference/operator/projection/elemMatch elemMatch
     */
    public static <T> Bson elemMatch(final SFunction<T,?> fieldName, final QueryChainWrapper<?, ?> queryChainWrapper) {
        return elemMatch(fieldName.getFieldNameLine(),queryChainWrapper);
    }

    /**
     * 为给定的元字段名称创建一个到给定字段名称的 $meta 投影。
     *
     * @param fieldName 字段名称
     * @param metaFieldName 元字段名称
     * @return $project
     * @since mongodb.driver.manual reference/operator/aggregation/meta/
     * @since 4.1
     * @see #metaTextScore(String)
     * @see #metaSearchScore(String)
     * @see #metaSearchHighlights(String)
     */
    public static Bson meta(final String fieldName, final String metaFieldName) {
        return new BsonDocument(fieldName, new BsonDocument("$meta", new BsonString(metaFieldName)));
    }

    /**
     * 为给定的元字段名称创建一个到给定字段名称的 $meta 投影。
     *
     * @param fieldName 字段名称
     * @param metaFieldName 元字段名称
     * @return $project
     * @since mongodb.driver.manual reference/operator/aggregation/meta/
     * @since 4.1
     * @see #metaTextScore(String)
     * @see #metaSearchScore(String)
     * @see #metaSearchHighlights(String)
     */
    public static <T> Bson meta(final SFunction<T,?> fieldName, final String metaFieldName) {
        return meta(fieldName.getFieldNameLine(),metaFieldName);
    }

    /**
     * 为给定的元字段名称创建一个到给定字段名称的 $meta 投影。
     *
     * @param fieldName 字段名称
     * @param metaFieldName 元字段名称
     * @return $project
     * @since mongodb.driver.manual reference/operator/aggregation/meta/
     * @since 4.1
     * @see #metaTextScore(String)
     * @see #metaSearchScore(String)
     * @see #metaSearchHighlights(String)
     */
    public static <T,R> Bson meta(final SFunction<T,?> fieldName, final SFunction<R,?> metaFieldName) {
        return meta(fieldName.getFieldNameLine(),metaFieldName.getFieldNameLine());
    }

    /**
     * 创建针对给定字段名称 textScore 的投影，用于文本查询。
     * 调用此方法相当于以{@code "textScore"}作为第二个参数调用{@link #meta(String, String)} 。
     *
     * @param fieldName 字段名称
     * @return $project
     * @see Filters#text(String, TextSearchOptions)
     * @since mongodb.driver.manual reference/operator/aggregation/meta/#text-score-metadata--meta---textscore- textScore
     */
    public static Bson metaTextScore(final String fieldName) {
        return meta(fieldName, "textScore");
    }

    /**
     * 创建针对给定字段名称 textScore 的投影，用于文本查询。
     * 调用此方法相当于以{@code "textScore"}作为第二个参数调用{@link #meta(String, String)} 。
     *
     * @param fieldName 字段名称
     * @return $project
     * @see Filters#text(String, TextSearchOptions)
     * @since mongodb.driver.manual reference/operator/aggregation/meta/#text-score-metadata--meta---textscore- textScore
     */
    public static <T> Bson metaTextScore(final SFunction<T,?> fieldName) {
        return meta(fieldName, "textScore");
    }

    /**
     * 创建对 searchScore 的给定字段名称的投影,
     * 以用于 {@link Aggregates#search(SearchOperator, SearchOptions)} / {@link Aggregates#search(SearchCollector, SearchOptions)}.
     * 调用此方法相当于使用{@code "searchScore"}作为第二个参数调用{@link #meta(String, String)}.
     *
     * @param fieldName 字段名称
     * @return $project
     * @since mongodb.atlas.manual atlas-search/scoring/ Scoring
     * @since 4.7
     */
    public static Bson metaSearchScore(final String fieldName) {
        return meta(fieldName, "searchScore");
    }

    /**
     * 创建对 searchScore 的给定字段名称的投影,
     * 以用于 {@link Aggregates#search(SearchOperator, SearchOptions)} / {@link Aggregates#search(SearchCollector, SearchOptions)}.
     * 调用此方法相当于使用{@code "searchScore"}作为第二个参数调用{@link #meta(String, String)}.
     *
     * @param fieldName 字段名称
     * @return $project
     * @since mongodb.atlas.manual atlas-search/scoring/ Scoring
     * @since 4.7
     */
    public static <T> Bson metaSearchScore(final SFunction<T,?> fieldName) {
        return meta(fieldName, "searchScore");
    }

    /**
     * 创建对 searchHighlights 的给定字段名称的投影
     * 以用于 {@link Aggregates#search(SearchOperator, SearchOptions)} / {@link Aggregates#search(SearchCollector, SearchOptions)}.
     * 用{@code "searchHighlights"}作为第二个参数调用{@link #meta(String, String)} 。
     *
     * @param fieldName the field name
     * @return $project
     * @see com.mongodb.client.model.search.SearchHighlight
     * @since mongodb.atlas.manual atlas-search/highlighting/ Highlighting
     * @since 4.7
     */
    public static Bson metaSearchHighlights(final String fieldName) {
        return meta(fieldName, "searchHighlights");
    }

    /**
     * 创建对 searchHighlights 的给定字段名称的投影
     * 以用于 {@link Aggregates#search(SearchOperator, SearchOptions)} / {@link Aggregates#search(SearchCollector, SearchOptions)}.
     * 用{@code "searchHighlights"}作为第二个参数调用{@link #meta(String, String)} 。
     *
     * @param fieldName 字段名称
     * @return $project
     * @see com.mongodb.client.model.search.SearchHighlight
     * @since mongodb.atlas.manual atlas-search/highlighting/ Highlighting
     * @since 4.7
     */
    public static <T> Bson metaSearchHighlights(final SFunction<T,?> fieldName) {
        return meta(fieldName, "searchHighlights");
    }

    /**
     * Creates a projection to the given field name of a slice of the array value of that field.
     *
     * @param fieldName the field name
     * @param limit     the number of elements to project.
     * @return $project
     * @since mongodb.driver.manual reference/operator/projection/slice Slice
     */
    public static Bson slice(final String fieldName, final int limit) {
        return new BsonDocument(fieldName, new BsonDocument("$slice", new BsonInt32(limit)));
    }

    /**
     * 为该字段的数组值片段的给定字段名称创建一个投影。
     *
     * @param fieldName 字段名称
     * @param limit     要投影的元素数量
     * @return $project
     * @since mongodb.driver.manual reference/operator/projection/slice Slice
     */
    public static <T> Bson slice(final SFunction<T,?> fieldName, final int limit) {
        return slice(fieldName.getFieldNameLine(), limit);
    }

    /**
     * 为该字段的数组值片段的给定字段名称创建一个投影。
     *
     * @param fieldName 字段名称
     * @param skip      应用限制之前要跳过的元素数量
     * @param limit     要投影的元素数量
     * @return $project
     * @since mongodb.driver.manual reference/operator/projection/slice Slice
     */
    public static Bson slice(final String fieldName, final int skip, final int limit) {
        return new BsonDocument(fieldName, new BsonDocument("$slice", new BsonArray(asList(new BsonInt32(skip), new BsonInt32(limit)))));
    }

    /**
     * 为该字段的数组值片段的给定字段名称创建一个投影。
     *
     * @param fieldName 字段名称
     * @param skip      应用限制之前要跳过的元素数
     * @param limit     要投影的元素数量
     * @return $project
     * @since mongodb.driver.manual reference/operator/projection/slice Slice
     */
    public static <T> Bson slice(final SFunction<T,?> fieldName, final int skip, final int limit) {
        return slice(fieldName.getFieldNameLine(), skip,limit);
    }

    /**
     * 创建一个投影，将投影列表合并为一个。如果有重复的键，则最后一个优先。
     *
     * @param projections 要合并的投影列表
     * @return the combined projection
     */
    public static Bson fields(final Bson... projections) {
        return fields(asList(projections));
    }

    /**
     * 创建一个投影，将投影列表合并为一个。如果有重复的键，则最后一个优先。
     *
     * @param projections 要合并的投影列表
     * @return the combined projection
     */
    public static Bson fields(final List<? extends Bson> projections) {
        notNull("projections", projections);
        return new FieldsProjection(projections);
    }

    private static class FieldsProjection implements Bson {
        private final List<? extends Bson> projections;

        FieldsProjection(final List<? extends Bson> projections) {
            this.projections = projections;
        }

        @Override
        public <TDocument> BsonDocument toBsonDocument(final Class<TDocument> documentClass, final CodecRegistry codecRegistry) {
            BsonDocument combinedDocument = new BsonDocument();
            for (Bson sort : projections) {
                BsonDocument sortDocument = sort.toBsonDocument(documentClass, codecRegistry);
                for (String key : sortDocument.keySet()) {
                    combinedDocument.remove(key);
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

            FieldsProjection that = (FieldsProjection) o;

            return Objects.equals(projections, that.projections);
        }

        @Override
        public int hashCode() {
            return projections != null ? projections.hashCode() : 0;
        }

        @Override
        public String toString() {
            return "Projections{"
                    + "projections=" + projections
                    + '}';
        }
    }


    private static class ElemMatchFilterProjection implements Bson {
        private final String fieldName;
        private final Bson filter;

        ElemMatchFilterProjection(final String fieldName, final Bson filter) {
            this.fieldName = fieldName;
            this.filter = filter;
        }

        @Override
        public <TDocument> BsonDocument toBsonDocument(final Class<TDocument> documentClass, final CodecRegistry codecRegistry) {
            return new BsonDocument(fieldName, new BsonDocument("$elemMatch", filter.toBsonDocument(documentClass, codecRegistry)));
        }

        @Override
        public boolean equals(final Object o) {
            if (this == o) {
                return true;
            }
            if (o == null || getClass() != o.getClass()) {
                return false;
            }

            ElemMatchFilterProjection that = (ElemMatchFilterProjection) o;

            if (!Objects.equals(fieldName, that.fieldName)) {
                return false;
            }
            return Objects.equals(filter, that.filter);
        }

        @Override
        public int hashCode() {
            int result = fieldName != null ? fieldName.hashCode() : 0;
            result = 31 * result + (filter != null ? filter.hashCode() : 0);
            return result;
        }

        @Override
        public String toString() {
            return "ElemMatch Projection{"
                    + "fieldName='" + fieldName + '\''
                    + ", filter=" + filter
                    + '}';
        }
    }

    private static Bson combine(final List<String> fieldNames, final BsonValue value) {
        BsonDocument document = new BsonDocument();
        for (String fieldName : fieldNames) {
            document.remove(fieldName);
            document.append(fieldName, value);
        }
        return document;
    }

}
