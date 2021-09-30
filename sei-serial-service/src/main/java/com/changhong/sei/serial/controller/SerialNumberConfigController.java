package com.changhong.sei.serial.controller;

import com.changhong.sei.core.dto.ResultData;
import com.changhong.sei.core.dto.serach.PageResult;
import com.changhong.sei.core.dto.serach.Search;
import com.changhong.sei.core.dto.serach.SearchFilter;
import com.changhong.sei.core.service.bo.OperateResultWithData;
import com.changhong.sei.serial.api.SerialNumberConfigApi;
import com.changhong.sei.serial.dto.BarCodeAssociateDto;
import com.changhong.sei.serial.entity.BarCodeAssociate;
import com.changhong.sei.serial.entity.IsolationRecord;
import com.changhong.sei.serial.entity.SerialNumberConfig;
import com.changhong.sei.serial.entity.enumclass.ConfigType;
import com.changhong.sei.serial.sdk.SerialUtils;
import com.changhong.sei.serial.sdk.entity.BarCodeDto;
import com.changhong.sei.serial.service.BarCodeAssociateService;
import com.changhong.sei.serial.service.SerialNumberConfigService;
import io.swagger.annotations.Api;
import org.apache.commons.collections.CollectionUtils;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Objects;

@RestController
@Api(value = "SerialNumberConfigApi", tags = "barCode查询api")
@RequestMapping("serialNumberConfig")
public class SerialNumberConfigController implements SerialNumberConfigApi {

    @Autowired
    private SerialNumberConfigService serialNumberConfigService;

    @Autowired
    private BarCodeAssociateService barCodeAssociateService;

    @PostMapping("save")
    public ResultData<SerialNumberConfig> save(@RequestBody SerialNumberConfig serialNumberConfig){
        OperateResultWithData<SerialNumberConfig> result = serialNumberConfigService.save(serialNumberConfig);
        if(result.getSuccess()){
            return ResultData.success(result.getData());
        }else {
            return ResultData.fail(result.getMessage());
        }

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
    public IsolationRecord findByClassName(@RequestParam String className, String isolation){
       return serialNumberConfigService.findByClassNameAndConfigType(className, ConfigType.CODE_TYPE,isolation);
    }

    @PostMapping("genAndSaveAssociate")
    public String genNumberAndSaveAssociate(@RequestBody BarCodeDto barCodeDto){
      return serialNumberConfigService.genNumberAndSaveAssociate(barCodeDto);
    }

    @GetMapping("getReferenceIdByBarCode")
    public ResultData<BarCodeAssociateDto> getReferenceIdByBarCode(@RequestParam String barCode){
        BarCodeAssociate barCodeAssociate = barCodeAssociateService.findByProperty("barCode",barCode);
        if(Objects.isNull(barCodeAssociate)){
            return ResultData.fail("未找到对应代码");
        }
        ModelMapper modelMapper = new ModelMapper();
        return ResultData.success(modelMapper.map(barCodeAssociate,BarCodeAssociateDto.class));
    }

    @GetMapping("getBarCodeListByReferenceId")
    public ResultData<List<BarCodeAssociateDto>> getBarCodeListByReferenceId(@RequestParam String referenceId){
        List<BarCodeAssociate> barCodeAssociates = barCodeAssociateService.findListByProperty("referenceId",referenceId);
        if(CollectionUtils.isEmpty(barCodeAssociates)){
            return ResultData.fail("未找到对应代码");
        }
        ModelMapper modelMapper = new ModelMapper();
        return ResultData.success(modelMapper.map(barCodeAssociates,new TypeToken<List<BarCodeAssociateDto>>() {
        }.getType()));
    }

    @PostMapping("getReferenceIdsByListBarCode")
    public ResultData<List<BarCodeAssociateDto>> getReferenceIdsByListBarCode(@RequestBody List<String> barCodes){
        SearchFilter searchFilter = new SearchFilter();
        searchFilter.setFieldName("barCode");
        searchFilter.setOperator(SearchFilter.Operator.IN);
        searchFilter.setValue(barCodes);
        List<BarCodeAssociate> barCodeAssociates = barCodeAssociateService.findByFilter(searchFilter);
        if(CollectionUtils.isEmpty(barCodeAssociates)){
            return ResultData.fail("未找到对应代码");
        }
        ModelMapper modelMapper = new ModelMapper();
        return ResultData.success(modelMapper.map(barCodeAssociates,new TypeToken<List<BarCodeAssociateDto>>() {
        }.getType()));
    }




}