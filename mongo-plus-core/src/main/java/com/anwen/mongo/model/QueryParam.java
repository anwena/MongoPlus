package com.anwen.mongo.model;

import com.mongodb.BasicDBObject;
import org.bson.conversions.Bson;

import java.util.Objects;

/**
 * 查询拦截器需要的值
 *
 * @author JiaChaoYang
 **/
public class QueryParam {
    public QueryParam(Bson query, BasicDBObject projection, BasicDBObject sort) {
        this.query = query;
        this.projection = projection;
        this.sort = sort;
    }

    /**
     * 查询参数（第一个）
     * @author JiaChaoYang
     * @date 2024/3/17 0:15
    */
    private Bson query;

    /**
     * project（第二个）
     * @author JiaChaoYang
     * @date 2024/3/17 0:15
    */
    private BasicDBObject projection;

    /**
     * 排序（第三个）
     * @author JiaChaoYang
     * @date 2024/3/17 0:15
    */
    private BasicDBObject sort;

    public Bson getQuery() {
        return query;
    }

    public void setQuery(Bson query) {
        this.query = query;
    }

    public BasicDBObject getProjection() {
        return projection;
    }

    public void setProjection(BasicDBObject projection) {
        this.projection = projection;
    }

    public BasicDBObject getSort() {
        return sort;
    }

    public void setSort(BasicDBObject sort) {
        this.sort = sort;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof QueryParam)) return false;
        QueryParam that = (QueryParam) o;
        return Objects.equals(getQuery(), that.getQuery()) && Objects.equals(getProjection(), that.getProjection()) && Objects.equals(getSort(), that.getSort());
    }

    @Override
    public int hashCode() {
        return Objects.hash(getQuery(), getProjection(), getSort());
    }

    @Override
    public String toString() {
        return "QueryParam{" +
                "query=" + query +
                ", projection=" + projection +
                ", sort=" + sort +
                '}';
    }
}
