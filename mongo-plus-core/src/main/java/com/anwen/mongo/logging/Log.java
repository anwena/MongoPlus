/*
 *    Copyright 2009-2022 the original author or authors.
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *       https://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */
package com.anwen.mongo.logging;

/**
 * @author Clinton Begin
 */
public interface Log {

  boolean isDebugEnabled();

  boolean isTraceEnabled();

  void error(String s, Throwable e);

  void error(String s,Object arg);

  void error(String s,Object arg1,Object arg2);

  void error(String s);

  void debug(String s);

  void debug(String format, Object arg);

  void debug(String format, Object arg1, Object arg2);

  void trace(String s);

  void warn(String s);

  void warn(String s,Object arg);

  void warn(String s,Object arg1,Object arg2);

}
