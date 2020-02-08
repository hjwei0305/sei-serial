package com.changhong.sei.serial.controller;

import com.changhong.sei.core.dto.ResultData;
import com.changhong.sei.core.dto.serach.PageResult;
import com.changhong.sei.core.dto.serach.Search;
import com.changhong.sei.core.service.bo.OperateResultWithData;
import com.changhong.sei.serial.entity.SerialNumberConfig;
import com.changhong.sei.serial.service.SerialNumberConfigService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("serialNumberConfig")
public class SerialNumberConfigController {

    @Autowired
    private SerialNumberConfigService serialNumberConfigService;

    @PostMapping("save")
    public ResultData<SerialNumberConfig> save(@RequestBody SerialNumberConfig serialNumberConfig){
        OperateResultWithData<SerialNumberConfig> result = serialNumberConfigService.save(serialNumberConfig);
        return ResultData.success(result.getData());
    }

    @PostMapping("findAll")
    public ResultData<PageResult> findAll(@RequestBody Search searchConfig){
        PageResult<SerialNumberConfig> result = serialNumberConfigService.findByPage(searchConfig);
        return ResultData.success(result);
    }

    @PostMapping("delete/{id}")
    public ResultData<?> delete(@PathVariable("id") String id){
        serialNumberConfigService.delete(id);
        return ResultData.success(true);
    }

    @GetMapping("findByClassName")
    public SerialNumberConfig findByClassName(@RequestParam String className,String isolationCode){
        return serialNumberConfigService.findByClassName(className,isolationCode);
    }

}