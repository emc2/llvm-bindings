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

-- | Utility functions for creating Range metadata.
module LLVM.Metadata.Range(
       rangeMetadata,
       rangeMetadataInContext
       ) where

maxSigned :: Integer -> Integer
maxSigned n = (2 ^ (n - 1)) - 1

-- | Possibly generate a range metadata node for value ranges for an
-- integer of the given size and signedness.  The list of intervals
-- must be ordered and non-overlapping, and in-bounds for the given
-- integer type.
-- 
-- Note: LLVM has no notion of signedness; the resulting metadata
-- orders intervals in signed order, even when the integer's
-- representation is unsigned.  This function will manage this
-- conversion; the interval list for an unsigned integer should
-- contain only unsigned values in unsigned order.
rangeMetadata :: Integer
              -- ^ Size of the integer type.
              -> Bool
              -- ^ Whether or not the ranges are for a signed or
              -- unsigned integer
              -> [(Integer, Integer)]
              -- ^ An unordered list of non-overlapping intervals,
              -- with (a, b) denoting the interval [a, b), or all n
              -- where a <= n < b.  It is required that a < b
              -> Maybe ValueRef
              -- ^ A metadata node, unless the ranges represent all
              -- possible values of integers of the given size.

-- | A version of rangeMetadata that creates metadata in a particular
-- context.
rangeMetadataInContext :: ContextRef
                       -- ^ The LLVM Context.
                       -> Integer
                       -- ^ Size of the integer type.
                       -> Bool
                       -- ^ Whether or not the ranges are for a signed
                       -- or unsigned integer
                       -> [(Integer, Integer)]
                       -- ^ An unordered list of non-overlapping
                       -- intervals, with (a, b) denoting the interval
                       -- [a, b), or all n where a <= n < b.  It is
                       -- required that a < b
                       -> Maybe ValueRef
                       -- ^ A metadata node, unless the ranges
                       -- represent all possible values of integers of
                       -- the given size.
