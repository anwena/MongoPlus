package com.uhu.controller;

import com.uhu.entity.User;
import com.uhu.mapper.UserMapper;
import org.noear.solon.annotation.Controller;
import org.noear.solon.annotation.Get;
import org.noear.solon.annotation.Mapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.Resource;
import java.util.List;

/**
 * @Description:
 * @Name: UserController
 * @Author: Bomber
 * @CreateTime: 2023/11/16 10:09
 */
@RestController
@Controller
public class UserController {

    @Resource
    private UserMapper userMapper;

    @Get
    @Mapping("/user/{id}")
    @GetMapping("/user/{id}")
    public User getById(@PathVariable Integer id) {
        return userMapper.getById(id);
    }

    @Get
    @Mapping("/one/{id}")
    @GetMapping("/one/{id}")
    public User test1(@PathVariable Integer id) {
        return userMapper.getOneById(id);
    }

    @Get
    @Mapping("/list/{id}")
    @GetMapping("/list/{id}")
    public List<User> test2(@PathVariable Integer id) {
        return userMapper.getList(id);
    }

    @Get
    @Mapping("/array/{id}")
    @GetMapping("/array/{id}")
    public User[] test3(@PathVariable Integer id) {
        return userMapper.getArray(id);
    }
}
