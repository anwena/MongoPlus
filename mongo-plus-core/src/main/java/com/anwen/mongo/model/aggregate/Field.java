package com.anwen.mongo.model.aggregate;

import com.anwen.mongo.support.SFunction;

/**
 * 管道的addFields阶段构建，支持lambda的Field
 * @param <TExpression> 值类型
 * @author anwen
 * @date 2024/6/7 下午2:35
 */
public class Field<TExpression> extends com.mongodb.client.model.Field<TExpression> {

    /**
     * 构建一个Field，会自动拼接$
     * @param name key
     * @param value value
     * @return {@link Field<TExpression>}
     * @author anwen
     * @date 2024/6/7 下午3:07
     */
    public static <T,R,TExpression> Field<TExpression> of(final SFunction<T,R> name, final TExpression value) {
        return new Field<>(true,name.getFieldNameLine(),value);
    }

    /**
     * 构建一个Field
     * @param name key
     * @param value value
     * @return {@link Field<TExpression>}
     * @author anwen
     * @date 2024/6/7 下午3:08
     */
    public static <TExpression> Field<TExpression> of(final String name, final TExpression value) {
        return new Field<>(false,name,value);
    }

    /**
     * 构建一个field，可选择是否拼接$
     * @param isField 是否是字段（是否拼接$）
     * @param name key
     * @param value value
     * @return {@link com.anwen.mongo.model.aggregate.Field<TExpression>}
     * @author anwen
     * @date 2024/6/7 下午3:08
     */
    public static <T,R,TExpression> Field<TExpression> of(final Boolean isField, final SFunction<T,R> name, final TExpression value) {
        return new Field<>(isField,name.getFieldNameLine(),value);
    }

    /**
     * 构建一个Field，不拼接$
     * @param name key
     * @param value value
     * @return {@link com.anwen.mongo.model.aggregate.Field<TExpression>}
     * @author anwen
     * @date 2024/6/7 下午3:09
     */
    public static <T,R,TExpression> Field<TExpression> ofNotField(final SFunction<T,R> name, final TExpression value) {
        return new Field<>(false,name.getFieldNameLine(),value);
    }

    public Field(final Boolean isField, String name, final TExpression value) {
        super(isField ? "$" + name : name,value);
    }

    public Field(String name, final TExpression value) {
        super(name,value);
    }

}
