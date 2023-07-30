package com.anwen.mongo.sql.conditions.interfaces;

import com.anwen.mongo.sql.conditions.interfaces.aggregate.project.Project;
import com.anwen.mongo.sql.support.SFunction;

public interface Query<T,Children> {

    Children projection(Project... project);

//    Children projection(SFunction<T,Object> column,Boolean value)


    /**
     * 正序排序
     * @param column 列名、字段名，lambda方式
     * @return com.anwen.mongo.sql.query.LambdaQueryMongoWrapper<T>
     * @author JiaChaoYang
     * @date 2023/6/20/020
     */
    Children orderByAsc(SFunction<T, Object> column);

    /**
     * 倒序排序
     * @param column 列名、字段名，lambda方式
     * @return com.anwen.mongo.sql.query.LambdaQueryMongoWrapper<T>
     * @author JiaChaoYang
     * @date 2023/6/20/020
     */
    Children orderByDesc(SFunction<T,Object> column);

    /**
     * 正序排序
     * @param column 列名、字段名
     * @return com.anwen.mongo.sql.query.LambdaQueryMongoWrapper<T>
     * @author JiaChaoYang
     * @date 2023/6/20/020
     */
    Children orderByAsc(String column);

    /**
     * 倒序排序
     * @param column 列名、字段名，lambda方式
     * @return com.anwen.mongo.sql.query.LambdaQueryMongoWrapper<T>
     * @author JiaChaoYang
     * @date 2023/6/20/020
     */
    Children orderByDesc(String column);

}
