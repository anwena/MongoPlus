package com.anwen.mongo.conditions.aggregate.aggregate;

import com.anwen.mongo.conditions.interfaces.aggregate.aggregate.Aggregate;
import com.anwen.mongo.constant.AggregationOperators;
import com.anwen.mongo.model.aggregate.Field;
import com.anwen.mongo.support.SFunction;
import com.mongodb.BasicDBObject;
import com.mongodb.client.model.Aggregates;
import com.mongodb.client.model.BucketAutoOptions;
import com.mongodb.client.model.BucketOptions;
import org.bson.conversions.Bson;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

@SuppressWarnings("unchecked")
public class AggregateChainWrapper<Children> implements Aggregate<Children> {

    /**
     * 定义管道条件集合，需要通过调用顺序控制管道的顺序
     */
    private final List<Bson> aggregateConditionList = new ArrayList<>();

    /**
     * 定义自己
     * @date 2024/6/7 下午3:56
     */
    protected final Children typedThis = (Children) this;

    @Override
    public Children addFields(String field, String value) {
        return addFields(Field.of(field,value));
    }

    @Override
    public <T,R> Children addFields(SFunction<T, R> field, String value) {
        return addFields(field.getFieldNameLine(),value);
    }

    @Override
    public <T,R> Children addFields(String value, SFunction<T,R>... field) {
        StringBuilder sb = new StringBuilder();
        for (SFunction<T,R> sFunction : field) {
            sb.append(sFunction.getFieldNameLine()).append(".");
        }
        sb.deleteCharAt(sb.length()-1);
        return addFields(sb.toString(),value);
    }

    @Override
    public <T,R> Children addFields(SFunction<T,R> field, Object value) {
        return addFields(Field.of(field,value));
    }

    @Override
    public <T,R> Children addFields(SFunction<T,R> field, Collection<?> value) {
        return addFields(field.getFieldNameLine(),value);
    }

    @Override
    public Children addFields(String field, Collection<?> value) {
        return addFields(Field.of(field, new BasicDBObject(AggregationOperators.CONCAT_ARRAYS, new ArrayList<Bson>() {{
            add(new BasicDBObject(AggregationOperators.FIELD + field, value));
        }})));
    }

    @Override
    public Children addFields(Field<?>... fields) {
        return addFields(new ArrayList<>(Arrays.asList(fields)));
    }

    @Override
    public Children addFields(List<Field<?>> fields) {
        return addFields(Aggregates.addFields(new ArrayList<>(fields)));
    }

    @Override
    public Children addFields(Bson bson) {
        return custom(bson);
    }

    @Override
    public Children set(String field, String value) {
        return set(Field.of(field,value));
    }

    @Override
    public <T,R> Children set(SFunction<T,R> field, String value) {
        return set(field.getFieldNameLine(),value);
    }

    @Override
    public <T,R> Children set(String value, SFunction<T,R>... field) {
        StringBuilder sb = new StringBuilder();
        for (SFunction<T,R> sFunction : field) {
            sb.append(sFunction.getFieldNameLine()).append(".");
        }
        sb.deleteCharAt(sb.length()-1);
        return set(sb.toString(),value);
    }

    @Override
    public <T,R> Children set(SFunction<T,R> field, Object value) {
        return set(Field.of(field,value));
    }

    @Override
    public <T,R> Children set(SFunction<T,R> field, Collection<?> value) {
        return set(field.getFieldNameLine(),value);
    }

    @Override
    public Children set(String field, Collection<?> value) {
        return set(Field.of(field, new BasicDBObject(AggregationOperators.CONCAT_ARRAYS, new ArrayList<Bson>() {{
            add(new BasicDBObject(AggregationOperators.FIELD + field, value));
        }})));
    }

    @Override
    public Children set(Field<?>... fields) {
        return set(new ArrayList<>(Arrays.asList(fields)));
    }

    @Override
    public Children set(List<Field<?>> fields) {
        return set(Aggregates.set(new ArrayList<>(fields)));
    }

    @Override
    public Children set(Bson bson) {
        return custom(bson);
    }

    @Override
    public <T,R,Boundary> Children bucket(SFunction<T,R> groupBy, List<Boundary> boundaries) {
        return bucket(groupBy.getFieldNameLineOption(),boundaries);
    }

    @Override
    public <Boundary> Children bucket(Object groupBy, List<Boundary> boundaries) {
        return bucket(Aggregates.bucket(groupBy,boundaries));
    }

    @Override
    public <T,R,Boundary> Children bucket(SFunction<T,R> groupBy, List<Boundary> boundaries, BucketOptions options) {
        return bucket(groupBy.getFieldNameLineOption(),boundaries,options);
    }

    @Override
    public <Boundary> Children bucket(Object groupBy, List<Boundary> boundaries, BucketOptions options) {
        return bucket(Aggregates.bucket(groupBy,boundaries,options));
    }

    @Override
    public Children bucket(Bson bson) {
        return custom(bson);
    }

    @Override
    public <T,R> Children bucketAuto(SFunction<T,R> groupBy, Integer buckets) {
        return bucketAuto(groupBy.getFieldNameLineOption(),buckets);
    }

    @Override
    public Children bucketAuto(Object groupBy, Integer buckets) {
        return bucketAuto(Aggregates.bucketAuto(groupBy,buckets));
    }

    @Override
    public <T,R> Children bucketAuto(SFunction<T,R> groupBy, Integer buckets, BucketAutoOptions options) {
        return bucketAuto(groupBy.getFieldNameLineOption(),buckets,options);
    }

    @Override
    public Children bucketAuto(Object groupBy, Integer buckets, BucketAutoOptions options) {
        return bucketAuto(Aggregates.bucketAuto(groupBy,buckets,options));
    }

    @Override
    public Children bucketAuto(Bson bson) {
        return custom(bson);
    }

    @Override
    public Children count() {
        return custom(Aggregates.count());
    }

    @Override
    public Children count(String field) {
        return custom(Aggregates.count(field));
    }

    @Override
    public <T, R> Children count(SFunction<T, R> field) {
        return count(field.getFieldNameLine());
    }

    @Override
    public Children custom(Bson bson) {
        this.aggregateConditionList.add(bson);
        return typedThis;
    }

}
