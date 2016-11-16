; ModuleID = 'hello.c'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.8.0"

@.str = private unnamed_addr constant [8 x i8] c"Hello, \00", align 1
@.str1 = private unnamed_addr constant [6 x i8] c"World\00", align 1

; Function Attrs: nounwind ssp uwtable
define i32 @main() #0 {
  %1 = alloca i32, align 4
  %str = alloca i8*, align 8
  store i32 0, i32* %1
  br label %2

; <label>:2                                       ; preds = %0, %14
  store i8* getelementptr inbounds ([8 x i8]* @.str, i32 0, i32 0), i8** %str, align 8
  %3 = load i8** %str, align 8
  %4 = call i64 @llvm.objectsize.i64.p0i8(i8* %3, i1 false)
  %5 = icmp ne i64 %4, -1
  br i1 %5, label %6, label %11

; <label>:6                                       ; preds = %2
  %7 = load i8** %str, align 8
  %8 = load i8** %str, align 8
  %9 = call i64 @llvm.objectsize.i64.p0i8(i8* %8, i1 false)
  %10 = call i8* @__strcat_chk(i8* %7, i8* getelementptr inbounds ([6 x i8]* @.str1, i32 0, i32 0), i64 %9) #4
  br label %14

; <label>:11                                      ; preds = %2
  %12 = load i8** %str, align 8
  %13 = call i8* @__inline_strcat_chk(i8* %12, i8* getelementptr inbounds ([6 x i8]* @.str1, i32 0, i32 0))
  br label %14

; <label>:14                                      ; preds = %11, %6
  %15 = phi i8* [ %10, %6 ], [ %13, %11 ]
  br label %2
                                                  ; No predecessors!
  %17 = load i32* %1
  ret i32 %17
}

; Function Attrs: nounwind readnone
declare i64 @llvm.objectsize.i64.p0i8(i8*, i1) #1

; Function Attrs: nounwind
declare i8* @__strcat_chk(i8*, i8*, i64) #2

; Function Attrs: inlinehint nounwind ssp uwtable
define internal i8* @__inline_strcat_chk(i8* noalias %__dest, i8* noalias %__src) #3 {
  %1 = alloca i8*, align 8
  %2 = alloca i8*, align 8
  store i8* %__dest, i8** %1, align 8
  store i8* %__src, i8** %2, align 8
  %3 = load i8** %1, align 8
  %4 = load i8** %2, align 8
  %5 = load i8** %1, align 8
  %6 = call i64 @llvm.objectsize.i64.p0i8(i8* %5, i1 false)
  %7 = call i8* @__strcat_chk(i8* %3, i8* %4, i64 %6) #4
  ret i8* %7
}

attributes #0 = { nounwind ssp uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind readnone }
attributes #2 = { nounwind "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { inlinehint nounwind ssp uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #4 = { nounwind }

!llvm.ident = !{!0}

!0 = metadata !{metadata !"Apple LLVM version 5.1 (clang-503.0.40) (based on LLVM 3.4svn)"}
