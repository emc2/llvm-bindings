-- Copyright (c) 2013 Eric McCorkle.  All rights reserved.
--
-- Portions of this code are derived from software originally written
-- by Brian O' Sullivan.  Comments are copied in part from software
-- produced by the LLVM Compiler Infrastructure project.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
--
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
--
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--
-- 3. Neither the name of the author nor the names of any contributors
--    may be used to endorse or promote products derived from this software
--    without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE AUTHORS AND CONTRIBUTORS ``AS IS''
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
-- TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
-- PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS
-- OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
-- USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
-- ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
-- OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.

-- | Utility functions for creating TBAA metadata.
module LLVM.Metadata.TBAA(
       tbaaRootMetadata,
       tbaaMetadata,
       tbaaRootMetadataInContext,
       tbaaMetadataInContext
       ) where

import LLVM.Core

tbaaRootMetadata :: String
                 -- ^ The name to give the root metadata.
                 -> IO ValueRef
                 -- ^ The TBAA metadata node.
tbaaRootMetadata name =
  do
    str <- mdString name
    mdNode [str]

tbaaMetadata :: String
             -- ^ The name of the type.
             -> ValueRef
             -- ^ The parent TBAA node.
             -> Bool
             -- ^ Whether or not the TBAA node represents a constant type.
             -> IO ValueRef
             -- ^ The TBAA metadata node.
tbaaMetadata name parent False =
  do
    str <- mdString name
    mdNode [str, parent]
tbaaMetadata name parent True =
  do
    str <- mdString name
    mdNode [str, parent, constInt int1Type 1 False]

tbaaRootMetadataInContext :: ContextRef
                          -- ^ The LLVM Context.
                          -> String
                          -- ^ The name to give the root metadata.
                          -> IO ValueRef
                          -- ^ The TBAA metadata node.
tbaaRootMetadataInContext ctx name =
  do
    str <- mdStringInContext ctx name
    mdNodeInContext ctx [str]

tbaaMetadataInContext :: ContextRef
                      -- ^ The LLVM Context.
                      -> String
                      -- ^ The name of the type.
                      -> ValueRef
                      -- ^ The parent TBAA node.
                      -> Bool
                      -- ^ Whether or not the TBAA node represents a
                      -- constant type.
                      -> IO ValueRef
                      -- ^ The TBAA metadata node.
tbaaMetadataInContext ctx name parent False =
  do
    str <- mdStringInContext ctx name
    mdNodeInContext ctx [str, parent]
tbaaMetadataInContext ctx name parent True =
  do
    str <- mdStringInContext ctx name
    ty <- int1TypeInContext ctx
    mdNodeInContext ctx [str, parent, constInt ty 1 False]
