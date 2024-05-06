package com.anwen.mongo.proxy;

import com.anwen.mongo.logging.Log;
import com.anwen.mongo.toolkit.IkunRandomUtil;

/**
 * log的代理
 * @author JiaChaoYang
 * @date 2024/5/2 下午1:37
 */
public class LogProxy implements Log {

    private final Log log;

    public LogProxy(Log log) {
        this.log = log;
    }

    @Override
    public boolean isDebugEnabled() {
        return log.isDebugEnabled();
    }

    @Override
    public boolean isTraceEnabled() {
        return log.isTraceEnabled();
    }

    @Override
    public void info(String s) {
        log.info(IkunRandomUtil.getRandomLog()+s);
    }

    @Override
    public void error(String s, Throwable e) {
        log.error(IkunRandomUtil.getRandomLog()+s,e);
    }

    @Override
    public void error(String s, Object arg) {
        log.error(IkunRandomUtil.getRandomLog()+s,arg);
    }

    @Override
    public void error(String s, Object arg1, Object arg2) {
        log.error(IkunRandomUtil.getRandomLog()+s,arg1,arg2);
    }

    @Override
    public void error(String s) {
        log.error(IkunRandomUtil.getRandomLog()+s);
    }

    @Override
    public void debug(String s) {
        log.debug(IkunRandomUtil.getRandomLog()+s);
    }

    @Override
    public void debug(String format, Object arg) {
        log.debug(IkunRandomUtil.getRandomLog()+format,arg);
    }

    @Override
    public void debug(String format, Object arg1, Object arg2) {
        log.debug(IkunRandomUtil.getRandomLog()+format,arg1,arg2);
    }

    @Override
    public void trace(String s) {
        log.trace(IkunRandomUtil.getRandomLog()+s);
    }

    @Override
    public void warn(String s) {
        log.warn(IkunRandomUtil.getRandomLog()+s);
    }

    @Override
    public void warn(String s, Object arg) {
        log.warn(IkunRandomUtil.getRandomLog()+s,arg);
    }

    @Override
    public void warn(String s, Object arg1, Object arg2) {
        log.warn(IkunRandomUtil.getRandomLog()+s,arg1,arg2);
    }
}
