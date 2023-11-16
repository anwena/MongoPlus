package com.uhu;

import com.anwen.mongo.annotation.MapperScan;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

/**
 * @Description:
 * @Name: Application
 * @Author: Bomber
 * @CreateTime: 2023/11/16 9:38
 */
@MapperScan(basePackages = "com.uhu.mapper")
@SpringBootApplication
public class Application {
    public static void main(String[] args) {
        SpringApplication.run(Application.class, args);
    }
}
