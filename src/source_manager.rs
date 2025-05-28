use std::fs::{self, File};
use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use std::collections::HashMap;
use crate::error::Error;

/// 源代码管理器，负责文件和目录的读取、写入和批量转换
pub struct SourceManager {
    /// 源文件路径
    pub source_paths: Vec<PathBuf>,
    /// 源文件内容缓存
    pub file_contents: HashMap<PathBuf, String>,
    /// 输出目录
    pub output_dir: PathBuf,
    /// 文件扩展名
    pub extension: String,
}

impl SourceManager {
    /// 创建新的源代码管理器
    pub fn new(source_paths: Vec<PathBuf>, output_dir: PathBuf) -> Self {
        SourceManager {
            source_paths,
            file_contents: HashMap::new(),
            output_dir,
            extension: ".gx".to_string(),
        }
    }
    
    /// 设置文件扩展名
    pub fn set_extension(&mut self, extension: &str) {
        self.extension = extension.to_string();
    }
    
    /// 读取单个文件
    pub fn read_file(&mut self, path: &PathBuf) -> Result<&str, Error> {
        if self.file_contents.contains_key(path) {
            return Ok(&self.file_contents[path]);
        }
        
        let mut file = File::open(path)
            .map_err(|e| Error::io(&format!("Failed to open file {}: {}", path.display(), e)))?;
        
        let mut content = String::new();
        file.read_to_string(&mut content)
            .map_err(|e| Error::io(&format!("Failed to read file {}: {}", path.display(), e)))?;
        
        self.file_contents.insert(path.clone(), content);
        
        Ok(&self.file_contents[path])
    }
    
    /// 写入输出文件
    pub fn write_output(&self, path: &PathBuf, content: &str) -> Result<(), Error> {
        // 确保目录存在
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent)
                .map_err(|e| Error::io(&format!("Failed to create directory {}: {}", parent.display(), e)))?;
        }
        
        let mut file = File::create(path)
            .map_err(|e| Error::io(&format!("Failed to create file {}: {}", path.display(), e)))?;
        
        file.write_all(content.as_bytes())
            .map_err(|e| Error::io(&format!("Failed to write to file {}: {}", path.display(), e)))?;
        
        Ok(())
    }
    
    /// 获取输出路径
    pub fn get_output_path(&self, input_path: &PathBuf) -> PathBuf {
        // 计算相对路径
        let relative_path = if input_path.starts_with(&self.source_paths[0]) {
            input_path.strip_prefix(&self.source_paths[0]).unwrap_or(input_path)
        } else {
            input_path
        };
        
        // 更改扩展名为 .go
        let file_name = relative_path.file_name().unwrap_or_default();
        let file_stem = relative_path.file_stem().unwrap_or_default();
        let new_file_name = format!("{}.go", file_stem.to_string_lossy());
        
        // 构建输出路径
        let mut output_path = self.output_dir.clone();
        if let Some(parent) = relative_path.parent() {
            output_path.push(parent);
        }
        output_path.push(new_file_name);
        
        output_path
    }
    
    /// 读取目录下所有 GolangX 文件
    pub fn read_directory(&mut self, dir: &PathBuf) -> Result<Vec<PathBuf>, Error> {
        let mut gx_files = Vec::new();
        
        self.collect_gx_files(dir, &mut gx_files)?;
        
        Ok(gx_files)
    }
    
    /// 递归收集目录下所有 GolangX 文件
    fn collect_gx_files(&self, dir: &PathBuf, files: &mut Vec<PathBuf>) -> Result<(), Error> {
        if !dir.is_dir() {
            return Err(Error::io(&format!("Not a directory: {}", dir.display())));
        }
        
        for entry in fs::read_dir(dir)
            .map_err(|e| Error::io(&format!("Failed to read directory {}: {}", dir.display(), e)))? {
            
            let entry = entry
                .map_err(|e| Error::io(&format!("Failed to read directory entry: {}", e)))?;
            
            let path = entry.path();
            
            if path.is_dir() {
                self.collect_gx_files(&path, files)?;
            } else if path.is_file() && self.is_gx_file(&path) {
                files.push(path);
            }
        }
        
        Ok(())
    }
    
    /// 检查文件是否是 GolangX 文件
    fn is_gx_file(&self, path: &PathBuf) -> bool {
        if let Some(ext) = path.extension() {
            ext.to_string_lossy() == self.extension.trim_start_matches('.')
        } else {
            false
        }
    }
    
    /// 批量转换所有源文件
    pub fn batch_convert<F>(&mut self, convert_fn: F) -> Result<ConversionReport, Error>
    where
        F: Fn(&mut Self, &PathBuf) -> Result<(), Error>
    {
        let mut report = ConversionReport::new();
        
        for source_path in &self.source_paths.clone() {
            if source_path.is_file() {
                if self.is_gx_file(source_path) {
                    match convert_fn(self, source_path) {
                        Ok(_) => {
                            report.successful_files.push(source_path.clone());
                        },
                        Err(e) => {
                            report.failed_files.push((source_path.clone(), e.to_string()));
                        }
                    }
                } else {
                    report.skipped_files.push((source_path.clone(), "Not a GolangX file".to_string()));
                }
            } else if source_path.is_dir() {
                match self.read_directory(source_path) {
                    Ok(files) => {
                        for file in files {
                            match convert_fn(self, &file) {
                                Ok(_) => {
                                    report.successful_files.push(file.clone());
                                },
                                Err(e) => {
                                    report.failed_files.push((file.clone(), e.to_string()));
                                }
                            }
                        }
                    },
                    Err(e) => {
                        report.failed_directories.push((source_path.clone(), e.to_string()));
                    }
                }
            } else {
                report.skipped_files.push((source_path.clone(), "Path does not exist".to_string()));
            }
        }
        
        Ok(report)
    }
    
    /// 并行批量转换
    pub fn parallel_batch_convert<F>(&mut self, convert_fn: F) -> Result<ConversionReport, Error>
    where
        F: Fn(&mut SourceManager, &PathBuf) -> Result<(), Error> + Send + Sync + 'static
    {
        use rayon::prelude::*;
        use std::sync::{Arc, Mutex};
        
        let report = Arc::new(Mutex::new(ConversionReport::new()));
        
        // 收集所有需要处理的文件
        let mut all_files = Vec::new();
        
        for source_path in &self.source_paths.clone() {
            if source_path.is_file() {
                if self.is_gx_file(source_path) {
                    all_files.push(source_path.clone());
                } else {
                    let mut report = report.lock().unwrap();
                    report.skipped_files.push((source_path.clone(), "Not a GolangX file".to_string()));
                }
            } else if source_path.is_dir() {
                match self.read_directory(source_path) {
                    Ok(files) => {
                        all_files.extend(files);
                    },
                    Err(e) => {
                        let mut report = report.lock().unwrap();
                        report.failed_directories.push((source_path.clone(), e.to_string()));
                    }
                }
            } else {
                let mut report = report.lock().unwrap();
                report.skipped_files.push((source_path.clone(), "Path does not exist".to_string()));
            }
        }
        
        // 并行处理所有文件
        all_files.par_iter().for_each(|file| {
            let mut local_source_manager = SourceManager::new(
                vec![file.clone()],
                self.output_dir.clone(),
            );
            local_source_manager.extension = self.extension.clone();
            
            match convert_fn(&mut local_source_manager, file) {
                Ok(_) => {
                    let mut report = report.lock().unwrap();
                    report.successful_files.push(file.clone());
                },
                Err(e) => {
                    let mut report = report.lock().unwrap();
                    report.failed_files.push((file.clone(), e.to_string()));
                }
            }
        });
        
        Ok(Arc::try_unwrap(report).unwrap().into_inner().unwrap())
    }
}

/// 转换报告，记录转换结果
pub struct ConversionReport {
    /// 成功转换的文件
    pub successful_files: Vec<PathBuf>,
    /// 转换失败的文件及原因
    pub failed_files: Vec<(PathBuf, String)>,
    /// 跳过的文件及原因
    pub skipped_files: Vec<(PathBuf, String)>,
    /// 处理失败的目录及原因
    pub failed_directories: Vec<(PathBuf, String)>,
}

impl ConversionReport {
    /// 创建新的转换报告
    pub fn new() -> Self {
        ConversionReport {
            successful_files: Vec::new(),
            failed_files: Vec::new(),
            skipped_files: Vec::new(),
            failed_directories: Vec::new(),
        }
    }
    
    /// 获取报告摘要
    pub fn summary(&self) -> String {
        let mut summary = String::new();
        
        summary.push_str(&format!("Conversion Report:\n"));
        summary.push_str(&format!("  Successful: {}\n", self.successful_files.len()));
        summary.push_str(&format!("  Failed: {}\n", self.failed_files.len()));
        summary.push_str(&format!("  Skipped: {}\n", self.skipped_files.len()));
        summary.push_str(&format!("  Failed Directories: {}\n", self.failed_directories.len()));
        
        summary
    }
    
    /// 获取详细报告
    pub fn detailed(&self) -> String {
        let mut report = String::new();
        
        report.push_str(&format!("Detailed Conversion Report:\n\n"));
        
        if !self.successful_files.is_empty() {
            report.push_str("Successful Files:\n");
            for file in &self.successful_files {
                report.push_str(&format!("  - {}\n", file.display()));
            }
            report.push_str("\n");
        }
        
        if !self.failed_files.is_empty() {
            report.push_str("Failed Files:\n");
            for (file, reason) in &self.failed_files {
                report.push_str(&format!("  - {} (Reason: {})\n", file.display(), reason));
            }
            report.push_str("\n");
        }
        
        if !self.skipped_files.is_empty() {
            report.push_str("Skipped Files:\n");
            for (file, reason) in &self.skipped_files {
                report.push_str(&format!("  - {} (Reason: {})\n", file.display(), reason));
            }
            report.push_str("\n");
        }
        
        if !self.failed_directories.is_empty() {
            report.push_str("Failed Directories:\n");
            for (dir, reason) in &self.failed_directories {
                report.push_str(&format!("  - {} (Reason: {})\n", dir.display(), reason));
            }
            report.push_str("\n");
        }
        
        report
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::tempdir;
    
    #[test]
    fn test_read_file() {
        let temp_dir = tempdir().unwrap();
        let file_path = temp_dir.path().join("test.gx");
        
        fs::write(&file_path, "package main").unwrap();
        
        let mut source_manager = SourceManager::new(vec![file_path.clone()], temp_dir.path().to_path_buf());
        
        let content = source_manager.read_file(&file_path).unwrap();
        assert_eq!(content, "package main");
    }
    
    #[test]
    fn test_get_output_path() {
        let temp_dir = tempdir().unwrap();
        let source_dir = temp_dir.path().join("src");
        let output_dir = temp_dir.path().join("out");
        
        fs::create_dir_all(&source_dir).unwrap();
        
        let file_path = source_dir.join("test.gx");
        
        let source_manager = SourceManager::new(vec![source_dir.clone()], output_dir.clone());
        
        let output_path = source_manager.get_output_path(&file_path);
        assert_eq!(output_path, output_dir.join("test.go"));
    }
    
    #[test]
    fn test_is_gx_file() {
        let source_manager = SourceManager::new(vec![], PathBuf::from("/tmp"));
        
        assert!(source_manager.is_gx_file(&PathBuf::from("test.gx")));
        assert!(!source_manager.is_gx_file(&PathBuf::from("test.go")));
        assert!(!source_manager.is_gx_file(&PathBuf::from("test")));
    }
    
    #[test]
    fn test_batch_convert() {
        let temp_dir = tempdir().unwrap();
        let source_dir = temp_dir.path().join("src");
        let output_dir = temp_dir.path().join("out");
        
        fs::create_dir_all(&source_dir).unwrap();
        
        let file1_path = source_dir.join("test1.gx");
        let file2_path = source_dir.join("test2.gx");
        let file3_path = source_dir.join("test3.txt");
        
        fs::write(&file1_path, "package main1").unwrap();
        fs::write(&file2_path, "package main2").unwrap();
        fs::write(&file3_path, "not a gx file").unwrap();
        
        let mut source_manager = SourceManager::new(vec![source_dir.clone()], output_dir.clone());
        
        let report = source_manager.batch_convert(|sm, path| {
            let content = sm.read_file(path)?;
            let output_path = sm.get_output_path(path);
            sm.write_output(&output_path, &format!("// Converted\n{}", content))
        }).unwrap();
        
        assert_eq!(report.successful_files.len(), 2);
        assert_eq!(report.skipped_files.len(), 0);
        assert_eq!(report.failed_files.len(), 0);
        
        let output1 = fs::read_to_string(output_dir.join("test1.go")).unwrap();
        let output2 = fs::read_to_string(output_dir.join("test2.go")).unwrap();
        
        assert_eq!(output1, "// Converted\npackage main1");
        assert_eq!(output2, "// Converted\npackage main2");
        
        assert!(!output_dir.join("test3.go").exists());
    }
}
