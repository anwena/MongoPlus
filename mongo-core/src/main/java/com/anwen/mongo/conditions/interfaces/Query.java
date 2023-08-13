package com.anwen.mongo.conditions.interfaces;

import com.anwen.mongo.conditions.interfaces.aggregate.project.Projection;
import com.anwen.mongo.support.SFunction;

import java.util.List;

public interface Query<T,Children> {

    /**
     * 要显示哪写字段或者不显示哪些字段
     * @param projection Projection对象
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/1 21:36
    */
    Children project(Projection... projection);

    /**
     * 要显示哪写字段或者不显示哪些字段
     * @param projectionList Projection集合
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/1 21:36
     */
    Children project(List<Projection> projectionList);

    /**
     * 显示哪些字段
     * @param column 列名，字段名
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/1 21:37
    */
    Children projectDisplay(SFunction<T,Object>... column);

    /**
     * 显示哪些字段
     * @param column 列名、字段名
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/1 21:37
    */
    Children projectDisplay(String... column);

    /**
     * 不显示哪些字段
     * @param column 列名、字段名
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/1 21:37
    */
    Children projectNone(SFunction<T,Object>... column);

    /**
     * 不显示哪些字段
     * @param column 列名、字段名
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/1 21:38
    */
    Children projectNone(String... column);

    /**
     * 要显示哪写字段或者不显示哪些字段
     * @param displayId 是否显示_id
     * @param projection
     * @param
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/1 21:36
     */
    Children project(boolean displayId,Projection... projection);

    /**
     * 显示哪些字段
     * @param displayId 是否显示_id
     * @param column 列名，字段名
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/1 21:37
     */
    Children projectDisplay(boolean displayId,SFunction<T,Object>... column);

    /**
     * 显示哪些字段
     * @param displayId 是否显示_id
     * @param column 列名、字段名
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/1 21:37
     */
    Children projectDisplay(boolean displayId,String... column);

    /**
     * 不显示哪些字段
     * @param displayId 是否显示_id
     * @param column 列名、字段名
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/1 21:37
     */
    Children projectNone(boolean displayId,SFunction<T,Object>... column);

    /**
     * 不显示哪些字段
     * @param displayId 是否显示_id
     * @param column 列名、字段名
     * @return Children
     * @author JiaChaoYang
     * @date 2023/8/1 21:38
     */
    Children projectNone(boolean displayId,String... column);


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
