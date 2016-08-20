package com.madgag.git.bfg.cleaner

import com.google.common.io.ByteStreams
import com.madgag.git.ThreadLocalObjectDatabaseResources
import com.madgag.git.bfg.model.FileName.ImplicitConversions._
import com.madgag.git.bfg.model.{BlobFileMode, TreeBlobEntry, TreeBlobs}
import com.madgag.textmatching.TextMatcher
import org.eclipse.jgit.lib.Constants._
import org.eclipse.jgit.lib.ObjectId
import org.eclipse.jgit.util.io.EolCanonicalizingInputStream

import scalax.io.Resource
/**
  * Created by thewolt on 20/08/16.
  */
abstract class FixCrlfConverter(fileNameMatcher: TextMatcher) extends TreeBlobModifier {

  val threadLocalObjectDBResources: ThreadLocalObjectDatabaseResources

  val charsetDetector: BlobCharsetDetector = QuickBlobCharsetDetector

  def isDirty(line: String) = line.endsWith("\r\n")

  override def fix(e: TreeBlobEntry): (BlobFileMode, ObjectId) = {
    val opt = for {
      loader         <- Some(threadLocalObjectDBResources.reader().open(e.objectId))
      streamResource <- Some(Resource.fromInputStream(loader.openStream()))
      charset        <- charsetDetector.charsetFor(e, streamResource)
      reader         <- Some(streamResource.reader(charset))
      if reader.lines(includeTerminator = true).exists(isDirty)
    } yield {
      val closable: EolCanonicalizingInputStream = new EolCanonicalizingInputStream(loader.openStream(), false)
      try {
        val oid = threadLocalObjectDBResources.inserter().insert(OBJ_BLOB, ByteStreams.toByteArray(closable))
        e.copy(objectId = oid)
      } finally {
        closable.close()
      }
    }

    opt.getOrElse(e).withoutName
  }

  override def apply(treeBlobs: TreeBlobs): TreeBlobs = {
    treeBlobs.entries.map { entry =>
      if(fileNameMatcher(entry.filename)) {
        memoisedCleaner(entry)
      } else {
        entry
      }
    }
  }
}