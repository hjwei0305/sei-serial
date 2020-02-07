package com.changhong.sei.serial.controller;

import com.changhong.sei.core.dto.ResultData;
import com.changhong.sei.serial.entity.SerialNumberConfig;
import com.changhong.sei.serial.service.SerialNumberConfigService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("serialNumberConfig")
public class SerialNumberConfigController {

    @Autowired
    private SerialNumberConfigService serialNumberConfigService;

    @PostMapping("save")
    public ResultData<SerialNumberConfig> save(@RequestBody SerialNumberConfig serialNumberConfig){
        serialNumberConfig = serialNumberConfigService.save(serialNumberConfig);
        return ResultData.success(serialNumberConfig);
    }

    @GetMapping("findAll")
    public ResultData<List<SerialNumberConfig>> findAll(){
        List<SerialNumberConfig> result = serialNumberConfigService.findAll();
        return ResultData.success(result);
    }

    @PostMapping("delete/{id}")
    public ResultData<?> activatedConfig(@PathVariable("id") String id){
        serialNumberConfigService.activatedConfig(id);
        return ResultData.success(true);
    }

    @GetMapping("findByClassName")
    public SerialNumberConfig findByClassName(@RequestParam String className){
        return serialNumberConfigService.findByClassName(className);
    }

}