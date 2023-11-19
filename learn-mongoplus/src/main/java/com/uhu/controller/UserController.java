package com.uhu.controller;

import com.uhu.entity.User;
import com.uhu.mapper.UserMapper;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RestController;

import javax.annotation.Resource;

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
    public User getById(@PathVariable Integer id) {
        return userMapper.getById(id);
    }
}
