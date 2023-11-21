package com.uhu.mapper;

import com.anwen.mongo.annotation.mapper.Param;
import com.anwen.mongo.annotation.mapper.Select;
import com.anwen.mongo.mapper.BaseMapper;
import com.uhu.entity.User;

import java.util.List;

/**
 * @Description:
 * @Name: UserMapper
 * @Author: Bomber
 * @CreateTime: 2023/11/16 14:31
 */

public interface UserMapper extends BaseMapper<User> {

    @Select("{_id: {$eq: #{id}}}")
    User getOneById(@Param("id") Integer id);

    @Select("{_id: {$gte: #{arg0}}}")
    List<User> getList(@Param Integer id);

    @Select("{_id: {$lte: #{arg0}}}")
    User[] getArray(@Param Integer id);
}
