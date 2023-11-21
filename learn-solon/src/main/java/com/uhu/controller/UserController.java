package com.uhu.controller;

import com.uhu.entity.User;
import com.uhu.mapper.UserMapper;
import org.noear.solon.annotation.*;

import java.util.List;

/**
 * @Description:
 * @Name: UserController
 * @Author: Bomber
 * @CreateTime: 2023/11/16 10:09
 */
@Controller
public class UserController {

    // @Inject
    private UserMapper userMapper;

    @Get
    @Mapping("/test")
    public String test() {
        return "hello world";
    }

    @Get
    @Mapping("/user/{id}")
    public User getById(@Path Integer id) {
        return userMapper.getById(id);
    }

    @Get
    @Mapping("/one/{id}")
    public User test1(@Path Integer id) {
        return userMapper.getOneById(id);
    }

    @Get
    @Mapping("/list/{id}")
    public List<User> test2(@Path Integer id) {
        return userMapper.getList(id);
    }

    @Get
    @Mapping("/array/{id}")
    public User[] test3(@Path Integer id) {
        return userMapper.getArray(id);
    }
}
