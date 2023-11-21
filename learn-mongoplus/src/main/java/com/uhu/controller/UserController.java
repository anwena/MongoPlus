package com.uhu.controller;

import com.uhu.entity.User;
import com.uhu.mapper.UserMapper;
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
public class UserController {

    @Resource
    private UserMapper userMapper;

    @GetMapping("/user/{id}")
    public User getById(
            @PathVariable Integer id) {
        return userMapper.getById(id);
    }

    @GetMapping("/one/{id}")
    public User test1(
            @PathVariable Integer id) {
        return userMapper.getOneById(id);
    }

    @GetMapping("/list/{id}")
    public List<User> test2(@PathVariable Integer id) {
        return userMapper.getList(id);
    }

    @GetMapping("/array/{id}")
    public User[] test3(@PathVariable Integer id) {
        return userMapper.getArray(id);
    }
}
