package com.anwen.mongo.sql.conditions.interfaces;

import com.anwen.mongo.sql.conditions.interfaces.aggregate.project.Projection;
import com.anwen.mongo.sql.support.SFunction;

public interface Query<T,Children> {

    /**
     * 要显示哪写字段或者不显示哪些字段
     * @param projection
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/1 21:36
    */
    Children projection(Projection... projection);

    /**
     * 显示哪些字段
     * @param column 列名，字段名
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/1 21:37
    */
    Children projectionDisplay(SFunction<T,Object>... column);

    /**
     * 显示哪些字段
     * @param column 列名、字段名
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/1 21:37
    */
    Children projectionDisplay(String... column);

    /**
     * 不显示哪些字段
     * @param column 列名、字段名
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/1 21:37
    */
    Children projectionNone(SFunction<T,Object>... column);

    /**
     * 不显示哪些字段
     * @param column 列名、字段名
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/1 21:38
    */
    Children projectionNone(String... column);

    /**
     * 要显示哪写字段或者不显示哪些字段
     * @param displayId 是否显示_id
     * @param projection
     * @param
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/1 21:36
     */
    Children projection(boolean displayId,Projection... projection);

    /**
     * 显示哪些字段
     * @param displayId 是否显示_id
     * @param column 列名，字段名
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/1 21:37
     */
    Children projectionDisplay(boolean displayId,SFunction<T,Object>... column);

    /**
     * 显示哪些字段
     * @param displayId 是否显示_id
     * @param column 列名、字段名
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/1 21:37
     */
    Children projectionDisplay(boolean displayId,String... column);

    /**
     * 不显示哪些字段
     * @param displayId 是否显示_id
     * @param column 列名、字段名
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/1 21:37
     */
    Children projectionNone(boolean displayId,SFunction<T,Object>... column);

    /**
     * 不显示哪些字段
     * @param displayId 是否显示_id
     * @param column 列名、字段名
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/1 21:38
     */
    Children projectionNone(boolean displayId,String... column);


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
