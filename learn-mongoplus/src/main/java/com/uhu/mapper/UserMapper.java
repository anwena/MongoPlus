package com.uhu.mapper;

import com.anwen.mongo.annotation.Mapper;
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

@Mapper
public interface UserMapper extends BaseMapper<User> {

    @Select("{_id: {$eq: #{id}}}")
    List<User> custom(@Param("id") Integer id);
}
