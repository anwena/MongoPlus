package com.anwen.mongo.conditions.interfaces.aggregate.pipeline;

import com.anwen.mongo.support.SFunction;
import com.mongodb.client.model.WindowOutputFields;
import com.mongodb.client.model.fill.*;
import com.mongodb.internal.client.model.AbstractConstructibleBsonElement;
import org.bson.Document;
import org.bson.conversions.Bson;

import static com.mongodb.assertions.Assertions.notNull;

/**
 * $fill阶段
 *
 * @author anwen
 * @date 2024/6/19 下午10:55
 */
public class FillField extends AbstractConstructibleBsonElement<FillField> implements FillOutputField,ValueFillOutputField,LocfFillOutputField, LinearFillOutputField {

    public FillField(String name, Bson value) {
        super(name, value);
    }

    public FillField(Bson baseElement, Bson appendedElementValue) {
        super(baseElement, appendedElementValue);
    }

    public FillField(Bson fill) {
        super(fill);
    }

    @Override
    public FillField newSelf(Bson baseElement, Bson appendedElementValue) {
        return new FillField(baseElement,appendedElementValue);
    }

    /**
     * 返回使用指定的 {@code 表达式} 的 {@link FillOutputField}.
     *
     * @param field 要填写的字段.
     * @param expression 表达方式.
     * @param <TExpression> {@code 表达式} 类型.
     * @return 请求的 {@link FillOutputField}.
     * @since mongodb.driver.manual core/document/#dot-notation Dot notation
     */
    public static <TExpression> ValueFillOutputField value(final String field, TExpression expression) {
        return new FillField(notNull("field", field),
                new Document("value", (notNull("expression", expression))));
    }

    /**
     * 返回使用指定的 {@code 表达式} 的 {@link FillOutputField}.
     *
     * @param field 要填写的字段.
     * @param expression 表达方式.
     * @param <TExpression> {@code 表达式} 类型.
     * @return 请求的 {@link FillOutputField}.
     * @since mongodb.driver.manual core/document/#dot-notation Dot notation
     */
    public static <TExpression,T> ValueFillOutputField value(final SFunction<T,?> field, TExpression expression) {
        return value(field.getFieldNameLine(),expression);
    }

    /**
     * 返回使用 {@link WindowOutputFields#locf(String, Object) locf} 方法的 {@link FillOutputField}.
     *
     * @param field 要填写的字段.
     * @return 请求的 {@link FillOutputField}.
     * @since mongodb.driver.manual core/document/#dot-notation Dot notation
     */
    public static LocfFillOutputField locf(final String field) {
        return new FillField(notNull("field", field),
                new Document("method", "locf"));
    }

    /**
     * 返回使用 {@link WindowOutputFields#locf(String, Object) locf} 方法的 {@link FillOutputField}.
     *
     * @param field 要填写的字段.
     * @return 请求的 {@link FillOutputField}.
     * @since mongodb.driver.manual core/document/#dot-notation Dot notation
     */
    public static <T> LocfFillOutputField locf(final SFunction<T,?> field) {
        return locf(field.getFieldNameLine());
    }

    /**
     * 返回一个使用 {@link WindowOutputFields#linearFill(String, Object) Linear} 方法的 {@link FillOutputField}.
     * <p>
     * {@linkplain FillOptions#sortBy(Bson) Sorting} 是必需的。</p>
     *
     * @param field 要填写的字段.
     * @return 请求的 {@link FillOutputField}.
     * @since mongodb.driver.manual core/document/#dot-notation Dot notation
     */
    public static LinearFillOutputField linear(final String field) {
        return new FillField(notNull("field", field),
                new Document("method", "linear"));
    }

    /**
     * 返回一个使用 {@link WindowOutputFields#linearFill(String, Object) Linear} 方法的 {@link FillOutputField}.
     * <p>
     * {@linkplain FillOptions#sortBy(Bson) Sorting} 是必需的。</p>
     *
     * @param field 要填写的字段.
     * @return 请求的 {@link FillOutputField}.
     * @since mongodb.driver.manual core/document/#dot-notation Dot notation
     */
    public static <T> LinearFillOutputField linear(final SFunction<T,?> field) {
        return linear(field.getFieldNameLine());
    }

    /**
     * 在没有构建器方法的情况下，从 {@link Bson} 创建 {@link FillOutputField}更好地满足您的需求.
     * 该方法不能用于验证语法.
     * <p>
     * <i>例子</i><br>
     * 以下代码创建两个功能等效的 {@link FillOutputField},
     * 尽管它们可能不{@linkplain Object#equals(Object) 相等}.
     * <pre>{@code
     *  FillOutputField field1 = FillOutputField.locf("fieldName");
     *  FillOutputField field2 = FillOutputField.of(new Document("fieldName", new Document("method", "locf")));
     * }</pre>
     *
     * @param fill A {@link Bson} 代表所需的 {@link FillOutputField}.
     * @return 请求的 {@link FillOutputField}.
     */
    public static FillField of(final Bson fill) {
        return new FillField(notNull("fill", fill));
    }
    
}
