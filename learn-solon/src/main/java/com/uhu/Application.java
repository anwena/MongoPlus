package com.uhu;

import org.noear.solon.Solon;
import org.noear.solon.annotation.SolonMain;

/**
 * @Description:
 * @Name: Application
 * @Author: Bomber
 * @CreateTime: 2023/11/16 9:38
 */

@SolonMain
public class Application {
    public static void main(String[] args) {
        Solon.start(Application.class, args);
    }
}
